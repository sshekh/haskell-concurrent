{-# LANGUAGE BangPatterns, MagicHash #-}

module Data.Concurrent.MSQueue (
  newq, enq, deq, nullq, LinkedQueue()
) where

import Data.IORef
import Data.Atomics

import System.IO
import GHC.Base   -- for sameMutVar
import GHC.Prim

import GHC.IORef
import GHC.STRef

-- pointer_t is !IORef (Node a), these have been made mutable because head and tail would need to change values,
-- Node has a data type and a pointer
data Node a = Null | Node a !(IORef (Node a))
nullN = newIORef Null

-- FIXME
instance Eq (Node a) where
  (==) Null Null = True
  (==) (Node _ (IORef (STRef mv1)))
       (Node _ (IORef (STRef mv2))) = sameMutVar# mv1 mv2
  (==) _          _           = False


data LinkedQueue a = LQ {
  head :: !(IORef (Node a))
, tail :: !(IORef (Node a))
}

newq :: IO (LinkedQueue a)
newq = do
  let
    newNode = Node () nullN
  head <- newIORef newNode
  tail <- newIORef newNode
  return (LQ head tail)

enq :: LinkedQueue a -> a -> IO()
enq queue@(LQ hptr tptr) val = do
  let
    newNode = Node val nullN
  loop :: IO()
  where loop = undefined



deq :: LinkedQueue a -> IO(Maybe a)
deq = undefined

nullq :: LinkedQueue q -> IO Bool
nullq = undefined
