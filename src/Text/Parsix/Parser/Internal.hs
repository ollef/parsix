{-# LANGUAGE CPP, OverloadedStrings, RankNTypes #-}
-- | This module exposes internals of the package: its API may change independently of the PVP-compliant version number.
module Text.Parsix.Parser.Internal where

import Control.Applicative
import Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail as Fail
#endif
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
import Text.Parsix.Internal
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

instance Semigroup a => Semigroup (Parser a) where
  (<>) = liftA2 (<>)

#if MIN_VERSION_base(4,11,0)
instance Monoid a => Monoid (Parser a) where
#else
instance (Monoid a, Semigroup a) => Monoid (Parser a) where
#endif
  mempty = pure mempty
  mappend = (<>)

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
#if !MIN_VERSION_base(4,13,0)
  fail = Fail.fail
#endif

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
      (\a -> s0 a . setExpected)
      s
      (e0 . setExpected)
      e
    where
      expectedText = Text.pack expected
      setExpected e = e { errorInfoExpected = Set.singleton expectedText }

  skipMany p = () <$ manyAccum (\_ _ -> []) p

  unexpected s = Parser
    $ \_s0 _s e0 _e _pos _hl _inp -> e0 $ failed $ "unexpected " <> Text.pack s

  eof = notFollowedBy anyChar <?> "end of input"

  notFollowedBy p = try (optional p >>= maybe (pure ()) (unexpected . show))

instance CharParsing Parser where
  satisfy f = Parser
    $ \_s0 s e0 _e pos hl inp ->
      if codeUnits pos < Unsafe.lengthWord16 inp then
        case Unsafe.iter inp $ codeUnits pos of
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
      ins pos pos' = IntervalMap.insert (rightOpen (codeUnits pos) (codeUnits pos')) h

instance LookAheadParsing Parser where
  lookAhead (Parser p) = Parser
    $ \s0 _ e0 e pos -> p s0 (\a err _pos _hl -> s0 a err) e0 e pos

runParser
  :: Parser a
  -> Text
  -> FilePath
  -> Position
  -> Result (a, Position, Highlights)
runParser (Parser p) inp file start = p
  (\res _ -> Success (res, start, mempty))
  (\res _ pos hl -> Success (res, pos, hl))
  (\err -> Failure $ Error err start inp mempty file)
  (\err pos hl -> Failure $ Error err pos inp hl file)
  start
  mempty
  inp

parseFromFile :: MonadIO m => Parser a -> FilePath -> m (Maybe a)
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
parseText p inp file = (\(a, _, _) -> a) <$> runParser p inp file mempty

-- | @parseString p i file@ runs a parser @p@ on @i@. @file@ is only used for
-- reporting errors.
parseString :: Parser a -> String -> FilePath -> Result a
parseString p s = parseText p $ Text.pack s

-- | Parse some input and print the result to the console.
parseTest :: (MonadIO m, Show a) => Parser a -> String -> m ()
parseTest p s = case parseText p (Text.pack s) "<interactive>" of
  Failure e -> liftIO $ putDoc $ prettyError e <> line
  Success a -> liftIO $ print a
