{-# LANGUAGE BangPatterns #-}

module Data.Concurrent.MSQueue (
  newq, enq, deq, nullq, LinkedQueue()
) where

import Data.IORef
import Data.Atomics

import System.IO
import GHC.Base   -- for sameMutVar
import GHC.Prim   

-- pointer_t is !IORef (Node a), these have been made mutable because head and tail would need to change values, 
-- Node has a data type and a pointer
data Node a = Null | Node a !(IORef (Node a))
null = newIORef Null

-- FIXME
instance Eq Node where
  (==) :: Node a -> Node a -> Bool
  (==) Null Null = True
  (==) (Node _ (IORef (STRef mv1)))
       (Node _ (IORef (STRef mv2))) = sameMutVar# mv1 mv2
  (==) _          _           = False


data LinkedQueue a = LQ {
  head :: !IORef(Node a)
, tail :: !IORef(Node a)
}

newq :: IO (LinkedQueue a)
newq = do
  newNode = Node () null
  return (LQ newNode newNode)

enq :: LinkedQueue a -> a -> IO()
enq queue@(LQ hptr tptr) val = do
  newNode = Node val null
  loop :: IO()



deq :: LinkedQueue a -> IO(Maybe a)

nullq :: LinkedQueue q -> IO Bool


