{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Text.Parsix.Position where

import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char

data Position = Position
  { bytes :: !Int
  , visualRow :: !Int
  , visualColumn :: !Int
  } deriving (Eq, Ord, Show)

start :: Position
start = Position 0 0 0

next :: Char -> Int -> Position -> Position
next !c !delta !pos = Position
  { bytes = bytes pos + delta
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

positionRow :: Position -> ByteString -> ByteString
positionRow pos bs = rows bs !! visualRow pos
  where
    rows :: ByteString -> [ByteString]
    rows = BS.split $ fromIntegral $ ord '\n'

showPosition :: Position -> ByteString -> String
showPosition pos bs
  = UTF8.toString (positionRow pos bs) ++ "\n"
  ++ replicate (visualColumn pos) ' ' ++ "^"
