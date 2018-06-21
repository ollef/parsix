{-# LANGUAGE BangPatterns #-}
module Text.Parsix.Highlight where

import Data.IntervalMap.FingerTree(IntervalMap)
import qualified Data.IntervalMap.FingerTree as IntervalMap
import Data.List
import Data.Semigroup
import Data.Text(Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.Text.Unsafe as Unsafe
import Text.Parser.Token.Highlight

type Highlights = IntervalMap Int Highlight

-- | Create a right-open interval
rightOpen :: Int -> Int -> IntervalMap.Interval Int
rightOpen !start !end = IntervalMap.Interval start $! end - 1

rightOpenView :: IntervalMap.Interval Int -> (Int, Int)
rightOpenView (IntervalMap.Interval !start !end) = (,) start $! end + 1

codeUnitSlice :: Int -> Int -> Text -> Text
codeUnitSlice start end text
  = Unsafe.takeWord16 (end' - start')
  $ Unsafe.dropWord16 start' text
  where
    start' = clamp start
    end' = clamp end
    clamp x = max 0 $ min x $ Unsafe.lengthWord16 text

highlightInterval
  :: Semigroup a
  => (Text -> a)
  -> (Highlight -> a -> a)
  -> Text
  -> Int
  -> Int
  -> Highlights
  -> a
highlightInterval textPart highlightPart input start end highlights = go start boundaries
  where
    boundaries
      = uniq
      $ sort
      [ i
      | (s, e) <- rightOpenView . fst <$> IntervalMap.intersections
          (rightOpen start end)
          highlights
      , i <- [max start s, min end e]
      ]

    part s e
      = foldr
        highlightPart
        (textPart $ codeUnitSlice s e input)
        (snd <$> IntervalMap.dominators (rightOpen s e) highlights)

    go s [] = part s end
    go s (e:bs) = part s e <> go e bs

    uniq :: Eq a => [a] -> [a]
    uniq [] = []
    uniq (x:xs) = uniq1 x xs

    uniq1 :: Eq a => a -> [a] -> [a]
    uniq1 x [] = [x]
    uniq1 x (y:ys)
      | x == y = uniq1 x ys
      | otherwise = x : uniq1 y ys

prettyInterval
  :: Text
  -> Int
  -> Int
  -> Highlights
  -> Doc Highlight
prettyInterval = highlightInterval pretty annotate

defaultStyle :: Highlight -> AnsiStyle
defaultStyle h = case h of
  Comment -> color Blue
  ReservedIdentifier -> color Magenta
  ReservedConstructor -> color Magenta
  EscapeCode -> color Magenta
  Operator -> color Yellow
  CharLiteral -> color Cyan
  StringLiteral -> color Cyan
  Constructor -> bold
  ReservedOperator -> color Yellow
  ConstructorOperator -> color Yellow
  ReservedConstructorOperator -> color Yellow
  _ -> mempty
