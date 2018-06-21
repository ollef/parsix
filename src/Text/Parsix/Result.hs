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
  { errorInfoReason :: Maybe Text
  , errorInfoExpected :: Set Text
  } deriving (Eq, Ord, Show)

prettyErrorInfo :: ErrorInfo -> Doc AnsiStyle
prettyErrorInfo (ErrorInfo (Just reason) expected)
  | Set.null expected = pretty reason
  | otherwise = pretty reason <> colon <+> "expected" <> colon
    <+> hsep (punctuate comma $ pretty <$> Set.toList expected)
prettyErrorInfo (ErrorInfo Nothing expected)
  | Set.null expected = mempty
  | otherwise = "expected" <> colon
    <+> hsep (punctuate comma $ pretty <$> Set.toList expected)

failed :: Text -> ErrorInfo
failed x = ErrorInfo (Just x) mempty

instance Monoid ErrorInfo where
  mempty = ErrorInfo empty mempty
  mappend = (<>)

instance Semigroup ErrorInfo where
  ErrorInfo r1 e1 <> ErrorInfo r2 e2
    = ErrorInfo (r1 <|> r2) (e1 <> e2)

data Result a
  = Success a
  | Failure Error
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Error = Error
  { errorInfo :: !ErrorInfo
  , errorPosition :: !Position
  , errorSourceText :: !Text
  , errorHighlights :: Highlights
  , errorFilePath :: FilePath
  } deriving (Eq, Ord, Show)

errorReason :: Error -> Maybe Text
errorReason = errorInfoReason . errorInfo

errorExpected :: Error -> Set Text
errorExpected = errorInfoExpected . errorInfo

prettyError :: Error -> Doc AnsiStyle
prettyError = prettyErrorWithStyle defaultStyle

prettyErrorWithStyle :: (Highlight -> AnsiStyle) -> Error -> Doc AnsiStyle
prettyErrorWithStyle style (Error info pos inp hl file)
  = (if null file then "" else pretty file <> ":")
  <> pretty (visualRow pos + 1) <> colon
  <> pretty (visualColumn pos + 1) <> colon <> line
  <> annotate (color Red) "error" <> colon <+> prettyErrorInfo info <> line
  <> prettyPosition style pos inp hl
