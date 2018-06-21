{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Text.Parsix.Position where

import Data.Semigroup
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
  } deriving (Eq, Ord, Show)

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
prettyPosition style pos inp hl
  = rowStringPadding <> bar <> line
  <> prettyRow <> bar <+> fmap style (positionRow pos inp hl) <> line
  <> rowStringPadding <> bar <+> pretty positionPadding <> annotate (color Red) "^"
  where
    barHighlight = annotate (color Blue)
    bar = barHighlight "|"
    prettyRow = barHighlight $ pretty rowString
    rowString = Text.pack (show $ visualRow pos + 1) <> " "
    rowStringPadding = pretty $ Text.replicate (Text.length rowString) " "

    positionPadding
      = Text.map go
      $ codeUnitSlice start end inp
      where
        start = prevNewline inp end
        end = codeUnits pos
        go '\t' = '\t'
        go _ = ' '

data Span = Span
  { spanStart :: !Position
  , spanEnd :: !Position
  } deriving (Eq, Ord, Show)
