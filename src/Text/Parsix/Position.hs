{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Text.Parsix.Position where

import Data.Semigroup
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.Text.Unsafe as Unsafe
import Text.Parser.Token.Highlight

import Text.Parsix.Highlight

data Position = Position
  { codePoints :: !Int
  , visualRow :: !Int
  , visualColumn :: !Int
  } deriving (Eq, Ord, Show)

next :: Char -> Int -> Position -> Position
next !c !delta !pos = Position
  { codePoints = codePoints pos + delta
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
    (prevNewline inp $ codePoints pos)
    (nextNewline inp $ codePoints pos)

positionPadding :: Position -> Text -> Text
positionPadding pos inp
  = Text.map go
  $ codePointSlice start end inp
  where
    start = prevNewline inp end
    end = codePoints pos
    go '\t' = '\t'
    go _ = ' '

nextNewline :: Text -> Int -> Int
nextNewline inp i = go inp i'
  where
    i' = max 0 i
    go text index
      | index >= len = len
      | otherwise = case Unsafe.iter text index of
        Unsafe.Iter '\n' _ -> index
        Unsafe.Iter _ delta -> nextNewline text $ index + delta
      where
        len = Unsafe.lengthWord16 text

prevNewline :: Text -> Int -> Int
prevNewline inp i = go inp (i' - 1) + 1
  where
    i' = min i $ Unsafe.lengthWord16 inp
    go text index
      | index < 0 = -1
      | otherwise = case Unsafe.reverseIter text index of
        ('\n', _) -> index
        (_, delta) -> go text $ index + delta

prettyPosition
  :: (Highlight -> AnsiStyle)
  -> Position
  -> Text
  -> Highlights
  -> Doc AnsiStyle
prettyPosition style pos inp hl
  = rowStringPadding <> bar <> line
  <> prettyRow <> bar <+> fmap style (positionRow pos inp hl) <> line
  <> rowStringPadding <> bar <+> pretty (positionPadding pos inp) <> annotate (color Red) "^"
  where
    barHighlight = annotate (color Blue)
    bar = barHighlight "|"
    prettyRow = barHighlight $ pretty rowString
    rowString = Text.pack (show $ visualRow pos + 1) <> " "
    rowStringPadding = pretty $ Text.replicate (Text.length rowString) " "

data Span = Span
  { spanStart :: !Position
  , spanEnd :: !Position
  } deriving (Eq, Ord, Show)
