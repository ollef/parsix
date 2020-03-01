{-# LANGUAGE CPP #-}
module Text.Parsix.Highlight where

import Data.IntervalMap.FingerTree(IntervalMap)
import qualified Data.IntervalMap.FingerTree as IntervalMap
import Data.List
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.Text(Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Text.Parser.Token.Highlight

import Text.Parsix.Internal

type Highlights = IntervalMap Int Highlight

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
