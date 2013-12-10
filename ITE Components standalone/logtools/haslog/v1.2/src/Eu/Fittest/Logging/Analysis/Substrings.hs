-- |
-- Searches for the positions of all substrings up to a certain length:
--   substrings keyinfo maxLength str
--
-- A string is a list of symbols of type |k|. A KeyInfo k value must
-- be provides. Such a value contains a function to compare symbols
-- for equality and a function to compute a hash from a symbol.
--
-- When two values are equal the hash values must also be equal.
-- The other way around is not necessary: it may only affect
-- performance. A hash function that returns a constant integer is
-- correct, yet of poor quality.
--
-- The implementation uses mutable datastructures under the hood
-- for performance reasons, such as hashtables using operations in
-- the IO-monad. The actual interface (e.g. the function
-- 'substrings') is purely functional.
--

{-# LANGUAGE BangPatterns #-}
module Eu.Fittest.Logging.Analysis.Substrings(substrings,substringsOfString) where

import Data.Char
import Control.Monad
import GHC.Base
import System.IO.Unsafe
import Data.HashTable(HashTable)
import qualified Data.HashTable as Tbl
import Data.IORef
import Data.Primitive.Array
import Data.List
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.IntSet(IntSet)
import qualified Data.IntSet as IntSet


--
-- Trie for fast searching through existing substrings
--

-- | A node in a trie. A node occurs in some context, the key, which is a sequence of 'k'
--   values.
data Trie k a                         -- keys: sequence of 'k', values: 'a'
  = Entry !(IORef a)                  -- a value
          !(HashTable k (Trie k a))   -- subtries

-- | Equality and a hash function on keys in the trie.
data KeyInfo a
  = KeyInfo { keyComp :: a -> a -> Bool, keyHash :: a -> Int }


-- | Inserts a key/value pair in the trie. The combination function is used when
--   there is already a value inserted with the same key.
insertWith :: KeyInfo k -> (a -> a -> a) -> [k] -> a -> Trie k a -> IO ()
insertWith info f = insert where
  insert [] v (Entry ref _) = modifyIORef ref (f v)
  insert (k:ks) v (Entry _ tbl) = do
    mb <- Tbl.lookup tbl k
    case mb of
      Just e  -> insert ks v e
      Nothing -> do e <- newTrie info v
                    Tbl.insert tbl k e

-- | Obtain the values from the trie. The results are returned in depth-first order.
--   Should probably replace this at some point with a breadth-first order.
elems :: Trie k a -> IO [([k], a)]
elems = elems' []

-- | With the context [k] as accumulator.
elems' :: [k] -> Trie k a -> IO [([k], a)]
elems' ks (Entry aRef tbl) = do
  a  <- readIORef aRef
  ts <- Tbl.toList tbl
  rs <- mapM (\(k, t) -> elems' (ks ++ [k]) t) ts
  return ((ks, a) : concat rs)

newTrie :: KeyInfo k -> a -> IO (Trie k a)
newTrie info v = do
  refKey <- newIORef v
  tbl    <- Tbl.newHint (keyComp info) (fromIntegral . keyHash info) 16  -- May want to make size 16 dependent on the dept.
  let !e = Entry refKey tbl
  return e


--
-- Buffer data structure
--

-- | A cyclic buffer data structure (queue). Items can be enqueued into the
--   buffer. Up to the last 'size' elements can be read from the buffer.
data Buffer a =
  Buffer  !Int            -- size of the buffer
          !(IORef Int)    -- total number of items enqueued (take it modulo size)
          !(MutableArray RealWorld a)  -- actual storage

-- | Note that buffer operations take pl
newBuffer :: Int -> IO (Buffer a)
newBuffer size = do
  lengthRef <- newIORef 0
  arr <- newArray size undefined
  let !buf = Buffer size lengthRef arr
  return buf

-- | Enqueue an item in the buffer.
advance :: Buffer a -> a -> IO ()
advance (Buffer size curRef arr) x = do
  length <- readIORef curRef
  let !length' = length + 1
  writeIORef curRef length'

  let !ind = length `mod` size
  writeArray arr ind x

-- | Obtain up to 'size' last enqueued items from the buffer.
--   When 'length' is the total number of enqueued items,
--   then the actual result is actually:
--   > length-1, [buffer ! length-1]
--   > length-2, [buffer ! length-1, buffer ! length-2 ]
--   > length-3, [buffer ! length-1, buffer ! length-2, buffer ! length-3 ]
--   > ...
--   > length-size, [buffer ! length-1, ..., buffer ! length-size ]
--   Thus, the results contains a previous position plus the items up to
--   the current position.
buffered :: Buffer a -> IO [(Int, [a])]
buffered (Buffer size curRef arr) = do
  length <- readIORef curRef
  let initial = max (length - size) 0
      items   = [initial .. length]
      indices = map (`mod` size) $ init items
  values <- mapM (readArray arr) indices
  return $ init $ zip items (tails values)


--
-- Construction of trie with symbols k as keys and sets of positions as values
--

-- | At each position, inserts up to the size of the buffer of elements into the
--   trie.
fill :: KeyInfo k -> Buffer k -> Trie k IntSet -> [k] -> IO ()
fill info buffer root = mapM_ inject where
  inject x = do
    advance buffer x
    seqs <- buffered buffer
    mapM_ augment seqs
  augment (pos,ks) = insertWith info IntSet.union ks (IntSet.singleton pos) root


--
-- Connect it together
--
-- | Returns the substrings up to 'maxLength' in the string of symbols [k] in arbitrary order.
--   In conjunction with each string, it returns the positions where the string was found.
--   The number of positions must be at least 'minOccur'.
substrings :: KeyInfo k -> Int -> Int -> [k] -> [([k], [Int])]
substrings info maxLength minOccur ks = unsafePerformIO (commons info maxLength minOccur ks)

-- | The underlying function which uses impure operations, but which together are
--   purely functional.
commons :: KeyInfo k -> Int -> Int -> [k] -> IO [([k], [Int])]
commons info maxLength minOccur ks = do
  buf  <- newBuffer maxLength
  trie <- newTrie info IntSet.empty
  fill info buf trie ks
  es <- elems trie
  return [ (ks, IntSet.elems ps) | (ks,ps) <- es, IntSet.size ps >= minOccur ]


--
-- Example using conventional strings
--

-- | Returns the substrings up to 'maxLength' in the string in arbitrary order that
--   at least occur twice.
substringsOfString :: Int -> String -> [(String, [Int])]
substringsOfString maxLength str = res where
  res  = substrings info maxLength 2 str
  info = KeyInfo (==) ord

test :: [(String, [Int])]
test = substringsOfString 3 "abcabcabc"
