module Worklist(worklist,Space(..),KeyInfo(..),Ops(..)) where

import Data.Int
import Data.Maybe
import Control.Monad
import Control.Applicative
import Data.HashTable(HashTable)
import qualified Data.HashTable as Tbl
import Data.Queue
import System.IO.Unsafe


data Space k v = Space { spGraph :: !(HashTable k [v]), spQueue :: !(TChan k) }
data KeyInfo k = KeyInfo { kiHash :: !(k -> Int32), kiCmp :: !(k -> k -> Bool) }
data Ops k v = Ops { opStep :: !(k -> [v]), opKey :: !(v -> k) }

initSpaceSize :: Int
initSpaceSize = 2 ^ 12

initSpace :: KeyInfo k -> IO (Space k v)
initSpace info = do
  table <- Tbl.newHint (kiCmp info) (kiHash info) initSpaceSize
  queue <- newFifo
  return Space { spGraph = table, spQueue = queue }

stepNext :: Ops k v -> Space k v -> IO Bool
stepNext ops (Space graph queue) = do
  mbKey <- dequeue queue
  case mbKey of
    Nothing -> return True
    Just k  -> do
      mbExists <- Tbl.lookup graph k
      case mbExists of
        Just _  -> return False
        Nothing -> do
          let vs = opStep ops k
              ks = map (opKey ops) vs
          Tbl.insert graph k vs
          enqueueBatch queue ks
          return False

construct :: Ops k v -> Space k v -> IO ()
construct ops space = rep where
  rep = do
    isEmpty <- stepNext ops space
    unless isEmpty rep

worklist :: KeyInfo k -> Ops k v -> [k] -> [(k, [v])]
worklist info ops inits = unsafePerformIO $ do
  space <- initSpace info
  enqueueBatch (spQueue space) inits
  construct ops space
  Tbl.toList (spGraph space)


test :: [(Int, [Int])]
test = worklist info ops [1] where
  info = KeyInfo fromIntegral (==)
  ops  = Ops step id
  step k = map (`mod` 10) [ k .. 2 * k ]
