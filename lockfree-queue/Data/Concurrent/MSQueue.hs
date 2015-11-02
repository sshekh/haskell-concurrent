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

next :: Node a -> !IORef( (Node a))
next Null = Null
next Node _ nptr = nptr

-- FIXME
instance Eq (Node a) where
  (==) Null Null = True
  (==) (Node _ (IORef (STRef mv1))) (Node _ (IORef (STRef mv2))) = sameMutVar# mv1 mv2
  (==) _          _           = False


data LinkedQueue a = LQ {
  head :: !(IORef (Node a))
  , tail :: !(IORef (Node a))
  }

newq :: IO (LinkedQueue a)
newq = do
  nullN <- newIORef Null
  let newNode = Node () nullN           -- Allocate a free node
  head <- newIORef newNode              -- Both head and tail point to it
  tail <- newIORef newNode
  return (LQ head tail)

enq :: LinkedQueue a -> a -> IO()
enq queue@(LQ hptr tptr) val = do
    nullN <- newIORef Null
    let newNode = Node val nullN          -- Allocate a new node
    loop
    where
      loop :: IO()
      loop = do
          tTicket <- readForCAS tptr        -- Read tail
          let tail = peekTicket tTicket
              nptr = next tTicket           -- Read next
          nTicket <- readForCAS nptr
          tTicket' <- readForCAS tptr
          if tail == peekTicket tTicket     -- are tail and next consistent?
            then case peekTicket nTicket of 
                      Null -> do (ret, newTicket) <- casIORef nptr nTicket newNode   -- try to link node at the end of the list
                                 if ret == True                                      -- enqueue done
                                    then do 
                                    _ <- casIORef tptr tTicket newNode             -- try to swing tail to inserted node
                                    return ()
                                    else loop
                      nxtN@(Node _ _) -> do
                        -- tail was not pointing to the last node, swing tptr
                        _ <- casIORef tptr tTicket nxtN
                        loop


--deq :: LinkedQueue a -> IO(Maybe a)
  

--nullq :: LinkedQueue q -> IO Bool
