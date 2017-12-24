{-# LANGUAGE RankNTypes #-}
module Text.Parsix where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import Data.Monoid
import qualified Data.Set as Set
import Data.Set(Set)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.LookAhead

-- | Input position in _bytes_
type Position = Int

data Error = Error
  { errorReason :: Maybe String
  , errorExpected :: Set String
  } deriving (Eq, Ord, Show)

failed :: String -> Error
failed x = Error (Just x) mempty

instance Monoid Error where
  mempty = Error empty mempty
  mappend (Error r1 e1) (Error r2 e2)
    = Error (r1 <|> r2) (e1 <> e2)

newtype Parser a = Parser
  { unParser
    :: forall r
    . (a -> r) -- success epsilon
    -> (a -> Position -> r) -- success committed
    -> (Error -> r) -- error epsilon
    -> (Error -> Position -> r) -- error committed
    -> Position
    -> ByteString
    -> r
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s0 s e0 e -> p (s0 . f) (s . f) e0 e

instance Applicative Parser where
  pure a = Parser $ \s0 _s _e0 _e _pos _bs -> s0 a
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

instance Monad Parser where
  return = pure
  Parser p >>= f = Parser
    $ \s0 s e0 e pos bs -> p
      (\a -> unParser (f a) s0 s e0 e pos bs)
      (\a pos' -> unParser (f a) (\b -> s b pos') s (\err -> e err pos') e pos' bs)
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
  $ \s0 s e0 e pos bs -> do
    let manyFailed = const
          $ e0 $ failed "'many' applied to a parser that accepts an empty string"
        walk xs x pos' = p
          manyFailed
          (walk $ f x xs)
          (\_err -> s (f x xs) pos')
          e
          pos'
          bs
    p manyFailed (walk []) (\_ -> s0 []) e pos bs

instance Parsing Parser where
  try (Parser p) = Parser
    $ \s0 s e0 _e pos -> p s0 s e0 (\_err _pos -> e0 mempty) pos

  Parser p <?> expected = Parser
    $ \s0 s e0 e -> p
      s0
      s
      (e0 . setExpected)
      (e . setExpected)
    where
      ex = Set.singleton expected
      setExpected e@(Error (Just _) _) = e
      setExpected (Error r _) = Error r ex

  skipMany p = () <$ manyAccum (\_ _ -> []) p

  unexpected s = Parser
    $ \_s0 _s e0 _e _pos _bs -> e0 $ failed $ "unexpected " ++ s

  eof = notFollowedBy anyChar <?> "end of input"

  notFollowedBy p = try (optional p >>= maybe (pure ()) (unexpected . show))

instance CharParsing Parser where
  satisfy f = Parser
    $ \_s0 s e0 _e pos bs -> case UTF8.decode $ BS.drop pos bs of
      Nothing -> e0 $ failed "Unexpected EOF"
      Just (c, delta)
        | f c -> s c $ pos + delta
        | otherwise -> e0 mempty

instance TokenParsing Parser

instance LookAheadParsing Parser where
  lookAhead (Parser p) = Parser
    $ \s0 s e0 e pos -> p s0 (\a _ -> s a pos) e0 e pos

recoveringWith :: Parser a -> (Error -> Parser a) -> Parser a
recoveringWith (Parser p) recover = Parser
  $ \s0 s e0 e pos bs -> p
    s0
    s
    (\err -> unParser (recover err) s0 s e0 e pos bs)
    (\err pos' -> unParser (recover err) s0 s e0 e pos' bs)
    pos
    bs

position :: Parser Position
position = Parser $ \s0 _s _e0 _e pos _bs -> s0 pos

input :: Parser ByteString
input = Parser $ \s0 _s _e0 _e _pos bs -> s0 bs

slicedWith :: (a -> ByteString -> b) -> Parser a -> Parser b
slicedWith f p = do
  i <- position
  a <- p
  j <- position
  bs <- input
  return $ f a $ BS.take (j - i) $ BS.drop i bs
