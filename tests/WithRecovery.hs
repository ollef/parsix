module WithRecovery where

import Test.Tasty.QuickCheck

import Util

tests :: TestTree
tests = testGroup "withRecovery"
  [ testProperty "inner epsilon success"
    $ \xs -> success (== Just ())
    $ parse (withRecovery (\_ _ -> pure Nothing) (pure (Just ()))) xs
  , testProperty "inner committed success"
    $ \x -> success (== Just x)
    $ parse (withRecovery (\_ _ -> pure Nothing) (Just <$> char x)) [x]
  , testProperty "inner epsilon error"
    $ \x y -> x /= y ==> success (== Nothing)
    $ parse (withRecovery (\_ _ -> pure Nothing) (Just <$> char x)) [y]
  , testProperty "inner committed error"
    $ \x y -> x /= y ==> success (== Nothing)
    $ parse (withRecovery (\_ _ -> pure Nothing) (Just <$> char x <* char x)) [x, y]
  , testProperty "recover epsilon error"
    $ \x y -> x /= y ==> failure (atPosition 1)
    $ parse (withRecovery (\_ _ -> empty) (Just <$> char x <* char x)) [x, y]
  , testProperty "recover committed error"
    $ \x y z -> x /= y && y /= z ==> failure (atPosition 1)
    $ parse (withRecovery (\_ _ -> Nothing <$ char y <* char y) (Just <$> char x <* char x)) [x, y, z]
  , testProperty "recovery runs from start"
    $ \x y -> x /= y ==> success (== Just x)
    $ parse (withRecovery (\_ _ -> Just <$> char x <* char y) (Just <$> char x <* char x)) [x, y]
  ]
