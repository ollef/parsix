{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Text.Parsix.Parser where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Control.Monad.IO.Class
import Data.Semigroup
import qualified Data.Set as Set
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Unsafe as Unsafe
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
    -> Position -- Input position
    -> Text -- Input
    -> r
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s0 s e0 e -> p (s0 . f) (s . f) e0 e

instance Applicative Parser where
  pure a = Parser $ \s0 _s _e0 _e _pos _inp -> s0 a mempty
  (<*>) = ap

instance Alternative Parser where
  empty = Parser $ \_s0 _s e0 _e _pos _inp -> e0 mempty
  Parser p <|> Parser q = Parser
    $ \s0 s e0 e pos inp -> p
      s0
      s
      (\err -> q s0 s (\err' -> e0 $ err <> err') e pos inp)
      e
      pos
      inp
  many p = reverse <$> manyAccum (:) p
  some p = (:) <$> p <*> many p

instance Monad Parser where
  return = pure
  Parser p >>= f = Parser
    $ \s0 s e0 e pos inp -> p
      (\a err -> unParser (f a)
        (\b err' -> s0 b $ err <> err')
        s
        (\err' -> e0 $ err <> err')
        e
        pos
        inp)
      (\a err pos' -> unParser (f a)
        (\b err' -> s b (err <> err') pos')
        s
        (\err' -> e (err <> err') pos')
        e
        pos'
        inp)
      e0
      e
      pos
      inp

instance MonadFail Parser where
  fail x = Parser
    $ \_s0 _s e0 _e _pos _inp -> e0 $ failed $ Text.pack x

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

manyAccum :: (a -> [a] -> [a]) -> Parser a -> Parser [a]
manyAccum f (Parser p) = Parser
  $ \s0 s _e0 e pos inp -> do
    let manyFailed pos' _ _ =
          e (failed "'many' applied to a parser that accepts an empty string") pos'
        walk xs x err pos' = p
          (manyFailed pos')
          (walk $ f x xs)
          (\err' -> s (f x xs) (err <> err') pos')
          e
          pos'
          inp
    p (manyFailed pos) (walk []) (s0 []) e pos inp

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
      expectedText = Text.pack expected
      addExpected e = e { errorExpected = Set.insert expectedText $ errorExpected e }

  skipMany p = () <$ manyAccum (\_ _ -> []) p

  unexpected s = Parser
    $ \_s0 _s e0 _e _pos _inp -> e0 $ failed $ "unexpected " <> Text.pack s

  eof = notFollowedBy anyChar <?> "end of input"

  notFollowedBy p = try (optional p >>= maybe (pure ()) (unexpected . show))

instance CharParsing Parser where
  satisfy f = Parser
    $ \_s0 s e0 _e pos inp ->
      if codePoints pos < Unsafe.lengthWord16 inp then
        case Unsafe.iter inp $ codePoints pos of
          Unsafe.Iter c delta
            | f c -> s c mempty $ next c delta pos
            | otherwise -> e0 mempty
      else
        e0 $ failed "Unexpected EOF"

instance TokenParsing Parser

instance LookAheadParsing Parser where
  lookAhead (Parser p) = Parser
    $ \s0 s e0 e pos -> p s0 (\a _ _ -> s a mempty pos) e0 e pos

parseFromFile :: MonadIO m => Parser a -> String -> m (Maybe a)
parseFromFile p file = do
  result <- parseFromFileEx p file
  case result of
   Success a  -> return $ Just a
   Failure xs -> do
     liftIO $ forM_ xs $ Text.putStrLn . showError
     return Nothing

parseFromFileEx :: MonadIO m => Parser a -> FilePath -> m (Result a)
parseFromFileEx p file = do
  s <- liftIO $ Text.readFile file
  return $ parseText p s file

-- | @parseText p i file@ runs a parser @p@ on @i@. @file@ is only used for
-- reporting errors.
parseText :: Parser a -> Text -> FilePath -> Result a
parseText (Parser p) inp file = p
  (\res _ -> Success res)
  (\res _ _pos -> Success res)
  (\err -> Failure $ pure $ Error err start inp file)
  (\err pos -> Failure $ pure $ Error err pos inp file)
  start
  inp
  where
    start = Position 0 0 0

parseString :: Parser a -> String -> FilePath -> Result a
parseString p s = parseText p $ Text.pack s

parseTest :: (MonadIO m, Show a) => Parser a -> String -> m ()
parseTest p s = case parseText p (Text.pack s) "<interactive>" of
  Failure xs -> liftIO $ forM_ xs $ Text.putStrLn . showError
  Success a  -> liftIO $ print a
