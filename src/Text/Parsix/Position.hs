{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Text.Parsix.Position where

import Data.Monoid
import Data.Text(Text)
import qualified Data.Text as Text

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

positionRow :: Position -> Text -> Text
positionRow pos bs = rows bs !! visualRow pos
  where
    rows :: Text -> [Text]
    rows = Text.splitOn "\n"

showPosition :: Position -> Text -> Text
showPosition pos inp
  = rowStringPadding <> "|\n"
  <> rowString <> "| " <> positionRow pos inp <> "\n"
  <> rowStringPadding <> "| " <> Text.replicate (visualColumn pos) " " <> "^"
  where
    rowString = Text.pack (show $ visualRow pos + 1) <> " "
    rowStringPadding = Text.replicate (Text.length rowString) " "
