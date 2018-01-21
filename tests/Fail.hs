module Fail where

import Test.Tasty.QuickCheck

import Util

tests :: TestTree
tests = testGroup "Failing parsers"
  [ testProperty "Fail" $ isFailure . parse (fail "fail")
  ]
