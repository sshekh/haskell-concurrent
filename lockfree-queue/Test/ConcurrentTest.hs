-- | concurrent test

module Test.ConcurrentTest where

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
  print x
  runThreadOps2 2500000 4
  putStrLn "Done"



forkThread :: IO () -> IO (MVar ())
forkThread proc = do
  handle <- newEmptyMVar
  _ <- forkFinally proc (\_ -> putMVar handle ())
  return handle


runThreadOps2 :: Int -> Int -> IO ()
runThreadOps2 m n = do
  a <- newq
  let enqNTimes a n v = do mapM (\x -> if v `mod` 2 == 1
                                         then enq a x
                                         else do deq a
                                                 return ()) [1..n]
                           return ()
  threads <- forM [1..n] (\v -> forkThread (enqNTimes a m v))
  mapM_ takeMVar threads
  x <- queueList a
  -- print x
  print (length x)
