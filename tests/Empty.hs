module Empty where

import Test.Tasty.QuickCheck

import Util

tests :: TestTree
tests = testGroup "Empty parsers"
  [ testProperty "mempty succeeds" $ Util.isSuccess . parse (mempty :: Parser ())
  , testProperty "empty fails" $ isFailure . parse empty
  ]
