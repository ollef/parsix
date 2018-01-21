module Util
  ( module Control.Applicative
  , module Control.Monad
  , module Data.Monoid
  , module Text.Parsix
  , module Test.Tasty
  , parse, isSuccess, isFailure, result, success, failure, atPosition
  ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.Text.Unsafe as Unsafe
import Test.Tasty

import Text.Parsix

parse :: Parser a -> String -> Result a
parse p s = parseString p s "<test input>"

isSuccess, isFailure :: Result a -> Bool
isSuccess Failure {} = False
isSuccess Success {} = True
isFailure Failure {} = True
isFailure Success {} = False

result :: (Error -> b) -> (a -> b) -> Result a -> b
result f _ (Failure e) = f e
result _ g (Success a) = g a

success :: (a -> Bool) -> Result a -> Bool
success = result $ const False

failure :: (Error -> Bool) -> Result a -> Bool
failure f = result f $ const False

atPosition :: Int -> Error -> Bool
atPosition pos err = go pos 0 == codePoints (errorPosition err)
  where
    inp = errorSourceText err
    go 0 cp = cp
    go n cp = go (n - 1) $ cp + Unsafe.iter_ inp cp
