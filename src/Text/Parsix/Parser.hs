{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Text.Parsix.Parser where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import qualified Data.IntervalMap.FingerTree as IntervalMap
import Data.Semigroup
import qualified Data.Set as Set
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.Text.Unsafe as Unsafe
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Token

import Text.Parsix.Highlight
import Text.Parsix.Position
import Text.Parsix.Result

newtype Parser a = Parser
  { unParser
    :: forall r
    . (a -> ErrorInfo -> r) -- success epsilon
    -> (a -> ErrorInfo -> Position -> Highlights -> r) -- success committed
    -> (ErrorInfo -> r) -- error epsilon
    -> (ErrorInfo -> Position -> Highlights -> r) -- error committed
    -> Position -- Input position
    -> Highlights -- Highlighting intervals
    -> Text -- Input
    -> r
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s0 s e0 e -> p (s0 . f) (s . f) e0 e

instance Applicative Parser where
  pure a = Parser $ \s0 _s _e0 _e _pos _hl _inp -> s0 a mempty
  (<*>) = ap

instance Alternative Parser where
  empty = Parser $ \_s0 _s e0 _e _pos _hl _inp -> e0 mempty
  Parser p <|> Parser q = Parser
    $ \s0 s e0 e pos hl inp -> p
      s0
      s
      (\err -> q s0 s (\err' -> e0 $ err <> err') e pos hl inp)
      e
      pos
      hl
      inp
  many p = reverse <$> manyAccum (:) p
  some p = (:) <$> p <*> many p

instance Monad Parser where
  return = pure
  Parser p >>= f = Parser
    $ \s0 s e0 e pos hl inp -> p
      (\a err -> unParser (f a)
        (\b err' -> s0 b $ err <> err')
        s
        (\err' -> e0 $ err <> err')
        e
        pos
        hl
        inp)
      (\a err pos' hl' -> unParser (f a)
        (\b err' -> s b (err <> err') pos' hl')
        s
        (\err' -> e (err <> err') pos' hl')
        e
        pos'
        hl'
        inp)
      e0
      e
      pos
      hl
      inp
  fail = Fail.fail

instance MonadFail Parser where
  fail x = Parser
    $ \_s0 _s e0 _e _pos _hl _inp -> e0 $ failed $ Text.pack x

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

manyAccum :: (a -> [a] -> [a]) -> Parser a -> Parser [a]
manyAccum f (Parser p) = Parser
  $ \s0 s _e0 e pos hl inp -> do
    let manyFailed pos' hl' _ _ =
          e (failed "'many' applied to a parser that accepts an empty string") pos' hl'
        walk xs x err pos' hl' = p
          (manyFailed pos' hl')
          (walk $ f x xs)
          (\err' -> s (f x xs) (err <> err') pos' hl')
          e
          pos'
          hl'
          inp
    p (manyFailed pos hl) (walk []) (s0 []) e pos hl inp

instance Parsing Parser where
  try (Parser p) = Parser
    $ \s0 s e0 _e -> p s0 s e0 (\_err _pos _hl -> e0 mempty)

  Parser p <?> expected = Parser
    $ \s0 s e0 e -> p
      (\a -> s0 a . addExpected)
      s
      (e0 . addExpected)
      e
    where
      expectedText = Text.pack expected
      addExpected e = e { errorInfoExpected = Set.insert expectedText $ errorInfoExpected e }

  skipMany p = () <$ manyAccum (\_ _ -> []) p

  unexpected s = Parser
    $ \_s0 _s e0 _e _pos _hl _inp -> e0 $ failed $ "unexpected " <> Text.pack s

  eof = notFollowedBy anyChar <?> "end of input"

  notFollowedBy p = try (optional p >>= maybe (pure ()) (unexpected . show))

instance CharParsing Parser where
  satisfy f = Parser
    $ \_s0 s e0 _e pos hl inp ->
      if codePoints pos < Unsafe.lengthWord16 inp then
        case Unsafe.iter inp $ codePoints pos of
          Unsafe.Iter c delta
            | f c -> s c mempty (next c delta pos) hl
            | otherwise -> e0 mempty
      else
        e0 $ failed "Unexpected EOF"

instance TokenParsing Parser where
  highlight h (Parser p) = Parser
    $ \s0 s e0 e pos -> p
      s0
      (\a err pos' -> s a err pos' . ins pos pos')
      e0
      (\err pos' -> e err pos' . ins pos pos')
      pos
    where
      ins pos pos' = IntervalMap.insert (rightOpen (codePoints pos) (codePoints pos')) h

instance LookAheadParsing Parser where
  lookAhead (Parser p) = Parser
    $ \s0 s e0 e pos -> p s0 (\a _ _ -> s a mempty pos) e0 e pos

parseFromFile :: MonadIO m => Parser a -> String -> m (Maybe a)
parseFromFile p file = do
  result <- parseFromFileEx p file
  case result of
   Success a -> return $ Just a
   Failure e -> do
     liftIO $ putDoc $ prettyError e <> line
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
  (\res _ _pos _hl -> Success res)
  (\err -> Failure $ Error err start inp mempty file)
  (\err pos hl -> Failure $ Error err pos inp hl file)
  start
  mempty
  inp
  where
    start = Position 0 0 0

parseString :: Parser a -> String -> FilePath -> Result a
parseString p s = parseText p $ Text.pack s

parseTest :: (MonadIO m, Show a) => Parser a -> String -> m ()
parseTest p s = case parseText p (Text.pack s) "<interactive>" of
  Failure e -> liftIO $ putDoc $ prettyError e <> line
  Success a -> liftIO $ print a
