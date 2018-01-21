module Main where

import qualified Empty
import qualified Fail
import qualified NotFollowedBy
import Util
import qualified WithRecovery

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ Empty.tests
  , Fail.tests
  , NotFollowedBy.tests
  , WithRecovery.tests
  ]
