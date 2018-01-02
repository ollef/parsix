{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, OverloadedStrings #-}
module Text.Parsix.Result where

import Control.Applicative
import Data.Semigroup
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Text(Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Text.Parser.Token.Highlight

import Text.Parsix.Position
import Text.Parsix.Highlight

data ErrorInfo = ErrorInfo
  { errorReason :: Maybe Text
  , errorExpected :: Set Text
  } deriving (Eq, Ord, Show)

prettyErrorInfo :: ErrorInfo -> Doc AnsiStyle
prettyErrorInfo (ErrorInfo mreason expected)
  = annotate (color Red) "error"
  <> maybe mempty (\reason -> colon <+> pretty reason) mreason
  <> if Set.null expected
    then mempty
    else maybe colon (const comma) mreason <+> "expected"
      <> colon <+> hsep (punctuate comma $ pretty <$> Set.toList expected)

failed :: Text -> ErrorInfo
failed x = ErrorInfo (Just x) mempty

instance Monoid ErrorInfo where
  mempty = ErrorInfo empty mempty
  mappend (ErrorInfo r1 e1) (ErrorInfo r2 e2)
    = ErrorInfo (r1 <|> r2) (e1 <> e2)

instance Semigroup ErrorInfo where

data Result a
  = Success a
  | Failure Error
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Error = Error !ErrorInfo !Position !Text Highlights FilePath
  deriving (Eq, Ord, Show)

prettyError :: (Highlight -> AnsiStyle) -> Error -> Doc AnsiStyle
prettyError style (Error info pos inp hl file)
  = (if null file then "" else pretty file <> ":")
  <> pretty (visualRow pos + 1) <> ":"
  <> pretty (visualColumn pos + 1) <> ": "
  <> prettyErrorInfo info <> line
  <> prettyPosition style pos inp hl
