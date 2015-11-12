{-# LANGUAGE BangPatterns, MagicHash, CPP #-}

module Data.Concurrent.MSQueue ( newq
                               , enq
                               , deq
                               , nullq
                               , LinkedQueue()
                               ) where

import Data.IORef
import Data.Atomics

import System.IO

import GHC.IORef
import GHC.STRef

#if MIN_VERSION_base(4,7,0)
import GHC.Base  hiding ((==#), sameMutVar#)
import GHC.Prim hiding ((==#), sameMutVar#)
import qualified GHC.PrimopWrappers as GPW
(==#) :: Int# -> Int# -> Bool
(==#) x y = case x GPW.==# y of { 0# -> False; _ -> True }

sameMutVar# :: MutVar# s a -> MutVar# s a -> Bool
sameMutVar# x y = case GPW.sameMutVar# x y of { 0# -> False; _ -> True }
#else
import GHC.Base
import GHC.Prim
#endif

-- pointer_t is !IORef (Node a), these have been made mutable because head and tail would need to change values,
-- Node has a data type and a pointer
data Node a = Null
            | Node { value :: a
                   , next :: !(IORef (Node a))}

-- next :: Node a -> Maybe (IORef (Node a))
-- next Null = Nothing
-- next (Node _ nptr) = Just nptr

-- value :: Node a -> Maybe a
-- value Null = Nothing
-- value (Node val _) = (Just val)

-- FIXME
instance Eq (Node a) where
  (==) Null Null = True
  (==) (Node _ (IORef (STRef mv1))) (Node _ (IORef (STRef mv2))) = sameMutVar# mv1 mv2
  (==) _          _           = False


data LinkedQueue a = LQ { head :: !(IORef (Node a))
                        , tail :: !(IORef (Node a))
                        }

newq :: IO (LinkedQueue a)
newq = do
  nullN <- newIORef Null
  let newNode = Node (error "Invalid node") nullN   -- Allocate a free node
  hd <- newIORef newNode              -- Both head and tail point to it
  tl <- newIORef newNode
  return (LQ hd tl)

nullq :: LinkedQueue q -> IO Bool
nullq queue@(LQ hptr tptr) = do
  head <- readIORef hptr
  tail <- readIORef tptr
  if head == tail
     then return True
     else return False

enq :: LinkedQueue a -> a -> IO()
enq queue@(LQ hptr tptr) val = do
    nullN <- newIORef Null
    let
      newNode = Node val nullN     -- Allocate a new node
                                   -- FIXME See if this works
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
    loop

deq :: LinkedQueue a -> IO(Maybe a)
deq queue@(LQ hptr tptr) = do
  loop
    where
      loop = do
      hTicket <- readForCAS hptr
      tTicket <- readForCAS tptr
      let head = peekTicket hTicket
          nptr = next head
      nTicket <- readForCAS nptr
      head' <- readIORef hptr
      if head == head'            -- are head and next consistent?
        then if head == peekTicket tTicket     -- is Queue empty or tail falling behind
                 then do
                   let nxtNode = peekTicket nTicket
                   if nxtNode == Null         -- Is Queue empty
                     then return Nothing     -- Queue empty
                     else do                 -- Tail falling behind, advance it
                       casIORef tptr tTicket nxtNode
                       loop
             else do                     -- No need to deal with tail
               let val = value nxtNode
                   nxtNode = peekTicket nTicket
               (ret, _) <- casIORef hptr hTicket nxtNode
               if ret == True
                 then return (Just val)
                 else loop
        else loop
