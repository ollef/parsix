module NotFollowedBy where

import Test.Tasty.QuickCheck

import Util

tests :: TestTree
tests = testGroup "notFollowedBy"
  [ testProperty "1"
    $ \x y -> result (\_ -> x == y) (/= y)
    $ parse (notFollowedBy (char y) *> anyChar) [x]
  , testProperty "2"
    $ \x -> isFailure
    $ parse (notFollowedBy (char x) *> anyChar) [x]
  , testProperty "3"
    $ \x y -> isFailure
    $ parse (anyChar *> notFollowedBy (char y) *> char y) x
  , testProperty "4"
    $ \x -> isSuccess
    $ parse (notFollowedBy (char x) <|> char x *> pure ()) [x]
  ]
