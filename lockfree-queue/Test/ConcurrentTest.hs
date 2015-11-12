-- | concurrent test

module Main where

import Test.Checks
import Test.QuickCheck
import Test.QuickCheck.Monadic(assert, monadicIO, run)

import Control.Monad
import Control.Concurrent
import Test.Helpers hiding (main)
import Data.Concurrent.MSQueue

import System.Environment

main :: IO()
main = do
  x <- getArgs
  let n = read $ x!!0
      m = read $ x!!1
  -- print x
  runThreadOps2 n m
  -- putStrLn "Done"



forkThread :: IO () -> IO (MVar ())
forkThread proc = do
  handle <- newEmptyMVar
  _ <- forkFinally proc (\_ -> putMVar handle ())
  return handle


runThreadOps2 :: Int -> Int -> IO ()
runThreadOps2 m n = do
  a <- newq
  let enqNTimes a n v = do mapM (\x -> if x `mod` 10 < 5 then enq a x
                                        else do deq a
                                                return ()) [1..n]
                           return ()
  threads <- forM [1..n] (\v -> forkThread (enqNTimes a m v))
  mapM_ takeMVar threads
  x <- queueList a
  -- print x
  -- print (length x)
  return ()

runThreadOps3 :: Int -> Int -> IO ()
runThreadOps3 m n = do
  a <- newq
  let enqNTimes a n v = do mapM (\x -> if v `mod` 2 == 1 then enq a x
                                        else do deq a
                                                return ()) [1..n]
                           return ()
  threads <- forM [1..n] (\v -> forkThread (enqNTimes a m v))
  mapM_ takeMVar threads
  x <- queueList a
  return ()
