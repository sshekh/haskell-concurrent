-- | Run all test

module Main where

import Test.Checks
import Test.QuickCheck
import Test.QuickCheck.Monadic(assert, monadicIO, run)


main :: IO()
main = do
  putStrLn "Zero Length newq"
  quickCheck zeroNewqCheck
  putStrLn "Simple Check for enq"
  quickCheck simpleCheck
  putStrLn "Correct deq behaviour"
  quickCheck dequeCorrectCheck
