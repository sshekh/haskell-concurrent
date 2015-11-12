-- | Test helpers

module Test.Helpers where

import Data.IORef
import Data.Atomics

import System.IO

import GHC.IORef
import GHC.STRef
import Data.Concurrent.MSQueue

import Test.QuickCheck
import Test.QuickCheck.Monadic(assert, monadicIO, run)

import Control.Concurrent

queueList :: LinkedQueue a -> IO [a]
queueList (LQ hd tl) = do
  hdNode <- readIORef hd
  let n = next hdNode
      toLst :: (IORef (Node a)) -> IO [a]
      toLst x = do a <- readIORef x
                   case a of
                     Null -> return []
                     (Node aa xx) -> do ls <- toLst xx
                                        return (aa:ls)
  l <- (toLst n)
  return l

listQueue :: [a] -> IO (LinkedQueue a)
listQueue [] = newq
listQueue (x:xs) = do
  a <- listQueue xs
  enq a x
  return a


main :: IO()
main = do
  a <- newq
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar
  x <- newEmptyMVar
  forkIO $ do
    enq a 1
    enq a 100
    _ <- takeMVar x
    enq a 1000
    enq a 10000
    putMVar m1 "Done 1"
  forkIO $ do
    enq a 200
    putMVar x "Test"
    enq a 300
    putMVar m2 "Done 2"
  v1 <- takeMVar m1
  putStrLn v1
  v2 <- takeMVar m2
  putStrLn v2
  x <- queueList a
  print x

runThreadOps :: Int -> Int -> IO()
runThreadOps m n = do
  a <- newq
  let runOps a n = do
        if n == 0 then return ()
          else do mm <- newEmptyMVar
                  forkIO $ do enqNTimes a m
                              runOps a (n-1)
                              putMVar mm ("Done" ++ show n)
                  x <- takeMVar mm
                  print x
                  return ()
      enqNTimes a n = do
        if n == 0 then return ()
          else do enq a (1+n)
                  enqNTimes a (n-1)
  runOps a n
  x <- queueList a
  print x
  print (length x)

