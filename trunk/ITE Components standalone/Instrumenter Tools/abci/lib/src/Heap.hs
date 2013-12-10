{-# LANGUAGE ExistentialQuantification, BangPatterns #-}
module Heap where

import Unsafe.Coerce
import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap


--
-- Heap and keys
--
-- Invariant: implicit bijection between uids and types.
-- The implementation assumes that if two keys have the
-- same uids, that the values have the same types.
-- This is not enforced by the implementation, but the
-- use of unsafeCoerce needs this constraint.
--

data Key a = Key
  { keyUid   :: !Int
  , keyInit  :: !(Maybe a)
  , keyJoin  :: !(a -> a -> a)
  , keyValEq :: !(a -> a -> Bool)
  }

instance Show (Key a) where
  show k = "Key-" ++ show (keyUid k)

data Threading = ThreadPrev | ThreadSucc | ThreadGlobal | ThreadLocal
  deriving (Eq,Ord,Show)

type Heap = IntMap HeapVal
data HeapVal = forall a . HeapVal
  { heapKey :: !(Key a)
  , heapVal :: !a
  }

fetchFromHeap :: Key a -> Heap -> Maybe a
fetchFromHeap (Key u m _ _) !h = case IntMap.lookup u h of
  Nothing            -> m
  Just (HeapVal _ v) -> Just $! unsafeCoerce v

storeInHeap :: Key a -> a -> Heap -> Heap
storeInHeap !k !v !h = IntMap.insertWith joinVals (keyUid k) (HeapVal k v) h

joinVals :: HeapVal -> HeapVal -> HeapVal
joinVals (HeapVal k@(Key p _ j _) a) (HeapVal (Key q _ _ _) b)
  | p == q    = HeapVal k (j a (unsafeCoerce b))
  | otherwise = error ("joinVals: cannot join key " ++ show p ++ " with key " ++ show q)

eqVals :: HeapVal -> HeapVal -> Bool
eqVals (HeapVal k@(Key p _ _ f) a) (HeapVal (Key q _ _ _) b)
  | p == q    = f a (unsafeCoerce b)
  | otherwise = False

eqHeaps :: Heap -> Heap -> Bool
eqHeaps !a !b = g1 && g2 && g3 where
  c  = IntMap.intersectionWith eqVals a b
  g1 = IntMap.size a == IntMap.size b
  g2 = IntMap.size a == IntMap.size c
  g3 = IntMap.fold (&&) True c

joinHeaps :: Heap -> Heap -> Heap
joinHeaps !a !b = IntMap.unionWith joinVals a b

joinHeaps' :: [Heap] -> Heap
joinHeaps' = IntMap.unionsWith joinVals
