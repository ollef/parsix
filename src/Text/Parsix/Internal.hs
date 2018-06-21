{-# LANGUAGE BangPatterns #-}
-- | This module exposes internals of the package: its API may change independently of the PVP-compliant version number.
module Text.Parsix.Internal where

import qualified Data.IntervalMap.FingerTree as IntervalMap
import Data.Text(Text)
import qualified Data.Text.Unsafe as Unsafe

codeUnitSlice :: Int -> Int -> Text -> Text
codeUnitSlice start end text
  = Unsafe.takeWord16 (end' - start')
  $ Unsafe.dropWord16 start' text
  where
    start' = clamp start
    end' = clamp end
    clamp x = max 0 $ min x $ Unsafe.lengthWord16 text

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

rightOpen :: Int -> Int -> IntervalMap.Interval Int
rightOpen !start !end = IntervalMap.Interval start $! end - 1

rightOpenView :: IntervalMap.Interval Int -> (Int, Int)
rightOpenView (IntervalMap.Interval !start !end) = (,) start $! end + 1
