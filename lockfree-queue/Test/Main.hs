-- | Run all test

module Main where

import Test.Checks
import Test.QuickCheck
import Test.QuickCheck.Monadic(assert, monadicIO, run)

import Control.Monad
import Control.Concurrent
import Test.Helpers hiding (main)
import Data.Concurrent.MSQueue

main :: IO()
main = do
  putStrLn "Zero Length newq"
  quickCheck zeroNewqCheck
  putStrLn "Simple Check for enq"
  quickCheck simpleCheck
  putStrLn "Correct deq behaviour"
  quickCheck dequeCorrectCheck
