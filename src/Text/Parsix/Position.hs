{-# LANGUAGE BangPatterns, DeriveGeneric, OverloadedStrings #-}
module Text.Parsix.Position where

import Data.Semigroup
import GHC.Generics
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Text.Parser.Token.Highlight

import Text.Parsix.Highlight
import Text.Parsix.Internal

data Position = Position
  { codeUnits :: !Int
  , visualRow :: !Int
  , visualColumn :: !Int
  } deriving (Eq, Ord, Show, Generic)

next :: Char -> Int -> Position -> Position
next !c !delta !pos = Position
  { codeUnits = codeUnits pos + delta
  , visualRow = row'
  , visualColumn = col'
  }
  where
    row = visualRow pos
    col = visualColumn pos
    (row', col') = case c of
      '\n' -> (row + 1, 0)
      '\t' -> (row, col + 8 - mod col 8)
      _ -> (row, col + 1)

positionRow :: Position -> Text -> Highlights -> Doc Highlight
positionRow pos inp
  = prettyInterval
    inp
    (prevNewline inp $ codeUnits pos)
    (nextNewline inp $ codeUnits pos)

prettyPosition
  :: (Highlight -> AnsiStyle)
  -> Position
  -> Text
  -> Highlights
  -> Doc AnsiStyle
prettyPosition style pos = prettySpan style $ Span pos pos

data Span = Span
  { spanStart :: !Position
  , spanEnd :: !Position
  } deriving (Eq, Ord, Show, Generic)

prettySpan
  :: (Highlight -> AnsiStyle)
  -> Span
  -> Text
  -> Highlights
  -> Doc AnsiStyle
prettySpan style (Span startPos endPos) inp hl
  = rowNumberStringPadding <> bar <> line
  <> prettyRowNumber <> bar <+> fmap style rowString <> line
  <> rowNumberStringPadding <> bar <+> pretty positionPadding <> annotate (color Red) ("^" <> pretty (Text.replicate squiggleLength "~"))
  where
    rowString = positionRow startPos inp hl
    barHighlight = annotate (color Blue)
    bar = barHighlight "|"
    prettyRowNumber = barHighlight $ pretty rowNumberString
    rowNumberString = Text.pack (show $ visualRow startPos + 1) <> " "
    rowNumberStringPadding = pretty $ Text.replicate (Text.length rowNumberString) " "

    positionPadding
      = Text.map go
      $ codeUnitSlice start end inp
      where
        start = prevNewline inp end
        end = codeUnits startPos
        go '\t' = '\t'
        go _ = ' '

    squiggleEnd =
      if visualRow endPos > visualRow startPos then
        nextNewline inp $ codeUnits startPos
      else
       codeUnits endPos
    squiggleLength = squiggleEnd - codeUnits startPos - 1
