{- 

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- |

  This module provides functions to compress a rawlog input and
  functions to select entries from compressed log without having
  to uncompressing it.
  
  Original implementation, using standard Map to represent the 
  working dictionary.
   
-}

{-# LANGUAGE BangPatterns #-}

module Eu.Fittest.Logging.Compression.Compress(
      Dictionary(..),
      SectionTag_(..),
      Paragraph_(..),
      CompressedTime(..),
      CompressedLogEntry(..),
      compressTimeStamp,
      decompressTimeStamp,
      strCompress,
      compress,
      filterLog,
      topfilterLog
  )

where

import Eu.Fittest.Logging.Compression.RawlogParser
import Data.IntMap
import Data.Map
import Data.HashTable
import Data.Maybe
import Data.Int
import Data.List
-- import Data.IORef
import System.IO

type DictionaryValue = [String]  
data IndexedDictionaryValue = IDV !Int32 DictionaryValue
        deriving
        (Eq,Show)        
  
indexifyDV sentences = IDV h sentences
    where
    h = hashString (concat sentences)
  
instance Ord IndexedDictionaryValue where
    compare (IDV h1 sentences1)
            (IDV h2 sentences2)
        = 
        compare (h1,sentences1) (h2,sentences2)

-- | This maps groups of sentences to unique integers.
type Dictionary = IntMap DictionaryValue

-- | An inverted version of Dictionary, used to build the dictionary.
type InvDictionary = Map IndexedDictionaryValue Int

invDict2Dict :: InvDictionary -> Dictionary
invDict2Dict invDict = Data.IntMap.fromList 
                       . Prelude.map (\(val,int)->(int, getSentences val)) 
                       . Data.Map.toList 
                       $ 
                       invDict
    where
    getSentences (IDV _ sentences) = sentences

data SectionTag_ = Tag_  String
                 | XTag_ Int
                 deriving (Eq,Show)
                 
data Paragraph_ = Par_ [String]
                | XPar_ Int
                deriving (Eq,Show)
   
data CompressedTime = 
       Time_   Int16 Int16 Int32   --offset, time/2^28, time%2^28   
     | Delta_  Int16               --difference with the previous timestamp
       deriving (Eq,Show)
       
int16UpBound::Integer
int16UpBound = 2^14

int16LowBound::Integer
int16LowBound = -int16UpBound

intUpBound::Integer
intUpBound = 2^28
    
compressTimeStamp1 :: UTCTimeStamp1970 ->  UTCTimeStamp1970 -> CompressedTime
compressTimeStamp1 (prevOffset,prevTime) (offset,time) =
    if (prevOffset==offset) 
        && int16LowBound<=delta
        && delta<=int16UpBound 
    then Delta_ (fromInteger delta)
    else Time_ (fromInteger . toInteger $ offset) (fromInteger part1) (fromInteger part2)
    where
    delta = time - prevTime
    part1 = time `div` intUpBound
    part2 = time `mod` intUpBound    
      
decompressTimeStamp1 :: UTCTimeStamp1970 -> CompressedTime -> UTCTimeStamp1970    
decompressTimeStamp1 (prevOffset,prevTime) (Delta_ d) = (prevOffset, prevTime + toInteger d)
decompressTimeStamp1 (prevOffset,prevTime) (Time_ offset part1 part2) = time2
    where
    time2 = (fromInteger . toInteger $ offset,
             (toInteger part1 * intUpBound) + toInteger part2)
      
-- | To compress the timestamps in a log      
compressTimeStamp log = snd . worker (0,0) $ log
    where
    worker prevtime [] = (prevtime, [])
    worker prevtime (Par p: rest) = (time2, Par p : rest2)
         where
         (time2, rest2) = worker prevtime rest
    worker prevtime (Section ts_ tag elems : rest) = (time3, new_section : rest3)
         where
         hasTimeStamp = isJust ts_
         t_ = compressTimeStamp1 prevtime (fromJust ts_)
         compressed_ts = if hasTimeStamp then Just t_ else Nothing
         new_prevtime  = if hasTimeStamp then fromJust ts_ else prevtime
         new_section = Section compressed_ts tag elems2
         (time2, elems2) = worker new_prevtime elems
         (time3, rest3)  = worker time2 rest
     
-- | To decompress the timestamps in a log     
decompressTimeStamp log = snd . worker (0,0) $ log
    where
    worker prevtime [] = (prevtime, [])
    worker prevtime (Par p: rest) = (time2, Par p : rest2)
         where
         (time2, rest2) = worker prevtime rest
    worker prevtime (Section ts_ tag elems : rest) = (time3, new_section : rest3)
         where
         hasTimeStamp = isJust ts_
         t_ = decompressTimeStamp1 prevtime (fromJust ts_)
         decompressed_ts = if hasTimeStamp then Just t_ else Nothing
         new_prevtime    = if hasTimeStamp then t_ else prevtime
         new_section = Section decompressed_ts tag elems2
         (time2, elems2) = worker new_prevtime elems
         (time3, rest3)  = worker time2 rest

-- | Representation of compressed log entries.      
type CompressedLogEntry = RawLogEntry CompressedTime SectionTag_ Paragraph_


-- | Compress a rawlog file. This returns a dictionary, and compressed log. 
compress :: String -> IO(Dictionary, [CompressedLogEntry])
compress file = do {
    handle <- openFile file ReadMode ;
    hSetEncoding handle utf8 ;
    content <- hGetContents handle ;
    result <- return (strCompress content) ;
    -- hClose handle ;
    return result ;
  }

-- | Compress a string containing rawlog. This returns a dictionary, 
--   and compressed log. 
strCompress :: String -> (Dictionary, [CompressedLogEntry])
strCompress inputstr = (invDict2Dict invDict, compressTimeStamp compressedLog)

  where
  
  rawlogEntries = strParseRawLog inputstr
  
  (_,invDict,compressedLog) = worker 0 (Data.Map.fromList []) rawlogEntries
  
  worker count invDict [] = (count, invDict, [])
  
  worker count invDict (Section ts_ tag elements : rest) 
         = 
         (count3, invDict3, new_section:compressed_rest)
      where    
      !tag_ = indexifyDV [tag]
      !number_   = Data.Map.lookup tag_ invDict
      !occurred   = isJust number_
      !new_count   = if occurred then count else count+1
      !new_invDict = if occurred then invDict else Data.Map.insert tag_ count invDict
      !new_tag     = if occurred then XTag_ (fromJust number_) else Tag_ tag
      
      !new_section = Section ts_ new_tag compressed_elements
      
      !(count2,invDict2,compressed_elements) = worker new_count new_invDict elements
      !(count3,invDict3,compressed_rest)     = worker count2 invDict2 rest
 
 
  worker count invDict (Par sentences : rest) 
      = 
      (count2,invDict2, new_par:compressed_rest)
      where
      !val_ = indexifyDV sentences
      !number_   = Data.Map.lookup val_ invDict
      !occurred  = isJust number_
      !new_count   = if occurred then count else count+1
      !new_invDict = if occurred then invDict else Data.Map.insert val_ count invDict
      !new_par     = if occurred then Par . XPar_ .fromJust $ number_ else Par (Par_ sentences)
      !(count2,invDict2,compressed_rest) = worker new_count new_invDict rest

-- | A function to filter compressed log, based on a predicate on the
-- timestamp and a predicate on the sections' tags. Entries satisfying
-- these predicates are kept, else they are dropped. This filter works
-- recursively on subsections.
--
filterLog timePred tagPred (dictionary,compressedLog) 
   = 
   compressTimeStamp . worker . decompressTimeStamp $  compressedLog
   where
   includedTagIndices = Data.IntMap.foldWithKey f [] dictionary 
   f key [tag] z = if tagPred tag then key:z else z   
   f _ _ z = z
   
   worker [] = []
   worker (Par p : rest) = Par p : worker rest
   worker (Section ts tag elems : rest) 
      =
      if timePred ts && tagPred_ tag 
        then (Section ts tag new_elems : new_rest)
        else  new_rest
      where
      tagPred_ (Tag_ tg) = tagPred tg
      tagPred_ (XTag_ i) = i `elem` includedTagIndices
      new_elems = worker elems  -- recurse on subsections
      new_rest  = worker rest   -- recurse on the tail of the log
      
-- | A function to filter compressed log, based on a predicate on the
-- timestamp and a predicate on the sections' tags. Entries satisfying
-- these predicates are kept, else they are dropped. This filter only
-- does the filtering on the top-sections. So, it does not filter
-- recursively on subsections.
topfilterLog timePred tagPred (dictionary,compressedLog) 
   = 
   compressTimeStamp . worker . decompressTimeStamp $  compressedLog
   where
   includedTagIndices = Data.IntMap.foldWithKey f [] dictionary 
   f key [tag] z = if tagPred tag then key:z else z   
   f _ _ z = z
   
   worker [] = []
   worker (Par p : rest) = Par p : worker rest
   worker (s@(Section ts tag elems) : rest) 
      =
      if timePred ts && tagPred_ tag 
        then (s : new_rest)
        else  new_rest
      where
      tagPred_ (Tag_ tg) = tagPred tg
      tagPred_ (XTag_ i) = i `elem` includedTagIndices
      new_rest  = worker rest   -- recurse only on the tail of the log 
      
--
-- Some tests
--
      
par1 = "%<P %<{ hello }%> %<{ world  }%> %>"
par2 = "%<P %<{ pussy }%> %<{ cat  }%> %>"
ex1 = par1 ++ par1 ++ par1 
ex2 = par1 ++ par2 ++ par1 ++ par2

sec1 = "%<S 120:1312203749927 \"first tag\" " ++ par1 ++ par2 ++ "%>"
sec2 = "%<S 120:1312203749929 \"second tag\" " ++ par1 ++ "%>"
ex3 = sec1 ++ sec1 ++ sec2
ex4 = sec1 ++ sec2 ++ sec1    


exLog1 = compress "./Eu/Fittest/Logging/Compression/SampleRawLog1.log" 
   
exFilter1 = do {
     (dict,log) <- exLog1 ;
     return (filter (dict,log)) 
   }
   where
   filter = filterLog (\t->True) (\tag-> "FX" `isPrefixOf` tag || "FE" `isPrefixOf` tag)

                     