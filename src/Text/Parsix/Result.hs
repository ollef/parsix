{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, OverloadedStrings #-}
module Text.Parsix.Result where

import Control.Applicative
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Semigroup
import qualified Data.Set as Set
import Data.Set(Set)

import Text.Parsix.Position

data ErrorInfo = ErrorInfo
  { errorReason :: Maybe Text
  , errorExpected :: Set Text
  } deriving (Eq, Ord, Show)

showErrorInfo :: ErrorInfo -> Text
showErrorInfo (ErrorInfo Nothing expected)
  | Set.null expected = ""
  | otherwise = "expected: " <> Text.intercalate ", " (Set.toList expected)
showErrorInfo (ErrorInfo (Just reason) expected)
  | Set.null expected = reason
  | otherwise = reason <> ",\nexpected: " <> Text.intercalate ", " (Set.toList expected)

failed :: Text -> ErrorInfo
failed x = ErrorInfo (Just x) mempty

instance Monoid ErrorInfo where
  mempty = ErrorInfo empty mempty
  mappend (ErrorInfo r1 e1) (ErrorInfo r2 e2)
    = ErrorInfo (r1 <|> r2) (e1 <> e2)

instance Semigroup ErrorInfo where

data Result a
  = Success a
  | Failure [Error]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Error = Error !ErrorInfo !Position !Text !(Maybe FilePath)
  deriving (Eq, Ord, Show)

instance Applicative Result where
  pure = Success

  Success f <*> Success a = Success $ f a
  Success {} <*> Failure e = Failure e
  Failure e <*> Success {} = Failure e
  Failure e <*> Failure e' = Failure (e <> e')

instance Alternative Result where
  empty = Failure mempty

  Failure xs <|> Failure ys = Failure $ xs <> ys
  s@Success {} <|> _ = s
  _ <|> s@Success {} = s

showError :: Error -> Text
showError (Error info pos bs mfile)
  = maybe "(interactive)" Text.pack mfile
  <> ":" <> shower (visualRow pos + 1)
  <> ":" <> shower (visualColumn pos + 1)
  <> ": " <> showErrorInfo info
  <> "\n"
  <> showPosition pos bs
    where
      shower = Text.pack . show
