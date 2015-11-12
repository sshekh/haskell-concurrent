-- | QuickCheck test

module Test.Checks where

import Test.QuickCheck
import Test.QuickCheck.Monadic(assert, monadicIO, run)
import Data.Concurrent.MSQueue
import Test.Helpers

simpleCheck :: [Int] -> Property
simpleCheck ls = monadicIO $ do
  a <- run $ listQueue ls
  k <- run $ queueList a
  assert $ (reverse k) == ls

zeroNewqCheck :: Property
zeroNewqCheck = monadicIO $ do
  a <- run $ newq
  l <- run $ queueList a
  assert $ (length l) == 0

dequeCorrectCheck :: [Int] -> Property
dequeCorrectCheck ls = monadicIO $ do
  a <- run $ listQueue (reverse ls)
  k <- run $ deq a
  let r = Prelude.head ls
  assert $ ((length ls) == 0) || (case k of
                                    Just p -> p == r
                                    Nothing -> False)

