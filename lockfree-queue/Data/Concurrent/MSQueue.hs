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

value :: Node a -> (Maybe a)
value Null = Nothing
value Node val _ = (Just val)

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
              nptr = next tail              -- Read next
          nTicket <- readForCAS nptr
          tail' <- readIORef tptr
          if tail == tail'                  -- are tail and next consistent?
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
          else
            return ()


--deq :: LinkedQueue a -> IO(Maybe a)
--deq queue@(LQ hptr tptr) = do
--  loop
--  where
--    loop :: IO(Maybe a) 
--    loop = do
--      hTicket <- readForCAS hptr
--      tTicket <- readForCAS tptr
--      let head = peekTicket hTicket
--          nptr = next head 
--      nTicket <- readForCAS nptr 
--      head' <- readIORef hptr
--      if head == head'            -- are head and next consistent?
--         then if head == peekTicket tTicket     -- is Queue empty or tail falling behind
--                 then do
--                   nxtNode <- peekTicket nTicket
--                   if nxtNode == Null         -- Is Queue empty
--                      then return Nothing     -- Queue empty
--                      else do                 -- Tail falling behind, advance it
--                       casIORef tptr tTicket nxtNode
--                       loop
--                  else do                     -- No need to deal with tail 
--                    nxtNode <- peekTicket nTicket                    
--                    let val = value nxtNode
      

--nullq :: LinkedQueue q -> IO Bool
