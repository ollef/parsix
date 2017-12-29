{-# LANGUAGE RankNTypes #-}
module Text.Parsix.Parser where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import Data.Semigroup
import qualified Data.Set as Set
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Token

import Text.Parsix.Position
import Text.Parsix.Result

newtype Parser a = Parser
  { unParser
    :: forall r
    . (a -> ErrorInfo -> r) -- success epsilon
    -> (a -> ErrorInfo -> Position -> r) -- success committed
    -> (ErrorInfo -> r) -- error epsilon
    -> (ErrorInfo -> Position -> r) -- error committed
    -> Position
    -> ByteString
    -> r
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s0 s e0 e -> p (s0 . f) (s . f) e0 e

instance Applicative Parser where
  pure a = Parser $ \s0 _s _e0 _e _pos _bs -> s0 a mempty
  (<*>) = ap

instance Alternative Parser where
  empty = Parser $ \_s0 _s e0 _e _pos _bs -> e0 mempty
  Parser p <|> Parser q = Parser
    $ \s0 s e0 e pos bs -> p
      s0
      s
      (\err -> q s0 s (\err' -> e0 $ err <> err') e pos bs)
      e
      pos
      bs
  many p = reverse <$> manyAccum (:) p
  some p = (:) <$> p <*> many p

instance Monad Parser where
  return = pure
  Parser p >>= f = Parser
    $ \s0 s e0 e pos bs -> p
      (\a err -> unParser (f a)
        (\b err' -> s0 b $ err <> err')
        s
        (\err' -> e0 $ err <> err')
        e
        pos
        bs)
      (\a err pos' -> unParser (f a)
        (\b err' -> s b (err <> err') pos')
        s
        (\err' -> e (err <> err') pos')
        e
        pos'
        bs)
      e0
      e
      pos
      bs

instance MonadFail Parser where
  fail x = Parser
    $ \_s0 _s e0 _e _pos _bs -> e0 $ failed x

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

manyAccum :: (a -> [a] -> [a]) -> Parser a -> Parser [a]
manyAccum f (Parser p) = Parser
  $ \s0 s _e0 e pos bs -> do
    let manyFailed pos' _ _ =
          e (failed "'many' applied to a parser that accepts an empty string") pos'
        walk xs x err pos' = p
          (manyFailed pos')
          (walk $ f x xs)
          (\err' -> s (f x xs) (err <> err') pos')
          e
          pos'
          bs
    p (manyFailed pos) (walk []) (s0 []) e pos bs

instance Parsing Parser where
  try (Parser p) = Parser
    $ \s0 s e0 _e pos -> p s0 s e0 (\_err _pos -> e0 mempty) pos

  Parser p <?> expected = Parser
    $ \s0 s e0 e -> p
      (\a -> s0 a . addExpected)
      s
      (e0 . addExpected)
      e
    where
      addExpected e = e { errorExpected = Set.insert expected $ errorExpected e }

  skipMany p = () <$ manyAccum (\_ _ -> []) p

  unexpected s = Parser
    $ \_s0 _s e0 _e _pos _bs -> e0 $ failed $ "unexpected " ++ s

  eof = notFollowedBy anyChar <?> "end of input"

  notFollowedBy p = try (optional p >>= maybe (pure ()) (unexpected . show))

instance CharParsing Parser where
  satisfy f = Parser
    $ \_s0 s e0 _e pos bs -> case UTF8.decode $ BS.drop (bytes pos) bs of
      Nothing -> e0 $ failed "Unexpected EOF"
      Just (c, delta)
        | f c -> s c mempty $ next c delta pos
        | otherwise -> e0 mempty

instance TokenParsing Parser

instance LookAheadParsing Parser where
  lookAhead (Parser p) = Parser
    $ \s0 s e0 e pos -> p s0 (\a _ _ -> s a mempty pos) e0 e pos

withRecovery :: Parser a -> (ErrorInfo -> Parser a) -> Parser a
withRecovery (Parser p) recover = Parser
  $ \s0 s e0 e pos bs -> p
    s0
    s
    e0
    (\err pos' -> unParser (recover err)
      (\a err' -> s a (err <> err') pos')
      s
      (\err' -> e (err <> err') pos')
      e
      pos'
      bs)
    pos
    bs

position :: Parser Position
position = Parser $ \s0 _s _e0 _e pos _bs -> s0 pos mempty

input :: Parser ByteString
input = Parser $ \s0 _s _e0 _e _pos bs -> s0 bs mempty

slicedWith :: (a -> ByteString -> b) -> Parser a -> Parser b
slicedWith f p = do
  i <- position
  a <- p
  j <- position
  bs <- input
  return $ f a $ BS.take (bytes j - bytes i) $ BS.drop (bytes i) bs

parseFromFile :: MonadIO m => Parser a -> String -> m (Maybe a)
parseFromFile p file = do
  result <- parseFromFileEx p file
  case result of
   Success a  -> return $ Just a
   Failure xs -> do
     liftIO $ forM_ xs $ putStrLn . showError
     return Nothing

parseFromFileEx :: MonadIO m => Parser a -> FilePath -> m (Result a)
parseFromFileEx p file = do
  s <- liftIO $ BS.readFile file
  return $ parseByteString p s $ Just file

-- | @parseByteString p i mfile@ runs a parser @p@ on @i@. @mfile@ is used
-- for reporting errors.
parseByteString :: Parser a -> UTF8.ByteString -> Maybe FilePath -> Result a
parseByteString (Parser p) bs mfile = p
  (\res _ -> Success res)
  (\res _ _pos -> Success res)
  (\err -> Failure $ pure $ Error err start bs mfile)
  (\err pos -> Failure $ pure $ Error err pos bs mfile)
  start
  bs

parseString :: Parser a -> String -> Maybe FilePath -> Result a
parseString p s = parseByteString p $ UTF8.fromString s

parseTest :: (MonadIO m, Show a) => Parser a -> String -> m ()
parseTest p s = case parseByteString p (UTF8.fromString s) Nothing of
  Failure xs -> liftIO $ forM_ xs $ putStrLn . showError
  Success a  -> liftIO $ print a
