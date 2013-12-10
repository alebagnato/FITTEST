{- 

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 

   This module provides functions to save a compressed log to a file,
   and to load the file again. The log is saved in a binary format.
     
-}

module Eu.Fittest.Logging.Compression.Serializer(
       saveCompressedLog,
       loadCompressedLog,
       compressThenSaveRawLog,
       saveFullyDecompressedLog
   )

where

import Eu.Fittest.Logging.Compression.Compress
import Eu.Fittest.Logging.Compression.RawlogParser
import Data.Binary
import Data.IntMap
import System.FilePath

instance Binary CompressedTime where
   put (Time_ offset part1 part2) = do {
          put (0 :: Word8) ;
          put offset ;
          put part1 ;
          put part2
       }
   put (Delta_  d) = do { put (1::Word8) ; put d }
   
   get = do { sel <- get :: Get Word8  ;
              case sel of
                0 -> do { offset <- get ;
                          p1 <- get ;
                          p2 <- get ;
                          return (Time_ offset p1 p2)
                        }
                1 -> do { d <- get ; return (Delta_ d) }
            }
   
instance Binary SectionTag_ where
   put (Tag_  s) = do { put (0 :: Word8)  ; put s }
   put (XTag_ i) = do { put (1 :: Word8)  ; put i }

   get = do { sel <- get :: Get Word8  ;
              case sel of
                0 -> do { s <- get ; return (Tag_ s)          }
                1 -> do { i <- get ; return (XTag_ i) }
            }
            
instance Binary Paragraph_ where
   put (Par_  z) = do { put (0 :: Word8)  ; put z }
   put (XPar_ i) = do { put (1 :: Word8)  ; put i }

   get = do { sel <- get :: Get Word8  ;
              case sel of
                0 -> do { z <- get ; return (Par_ z)          }
                1 -> do { i <- get ; return (XPar_ i) }
            }   

instance (Binary ts, Binary attr, Binary par) =>  Binary (RawLogEntry ts attr par)
   where 
   put (Section Nothing attr elems) = do {
           put (0::Word8) ;
           put attr ;
           put elems 
        }
   put (Section (Just t) attr elems) = do {
           put (1::Word8) ;
           put t ;
           put attr ;
           put elems 
        }     
   put (Par par) = do { put (2::Word8) ; put par }
   
   get = do { sel <- get :: Get Word8  ;
              case sel of
                0 -> do { attr <- get ; 
                          elems <- get ;
                          return (Section Nothing attr elems)
                        }
                1 -> do { t <- get ; 
                          attr <- get ; 
                          elems <- get ;
                          return (Section (Just t) attr elems)
                        }
                2 -> do { par <- get ; return (Par par) }
            }

-- | Save a dictionary and compressed log in a binary file. 
saveCompressedLog :: FilePath -> (Dictionary,[CompressedLogEntry]) -> IO()
saveCompressedLog file (dict,log) = do {
       encodeFile logf log ;
       encodeFile dicf (toList dict) ;
    }
    where
    basef = dropExtension file
    logf = addExtension basef "lox"
    dicf = addExtension basef "dic"
    
-- | Compress a rawlog file, and save it in a binary file.
compressThenSaveRawLog :: FilePath -> IO()
compressThenSaveRawLog file = do {
      (dict,log) <- compress file ;
      saveCompressedLog file (dict,log) 
   }    
    
-- | Load a compressed log and its dictionary  from their save files.  
loadCompressedLog :: FilePath -> IO(Dictionary,[CompressedLogEntry])
loadCompressedLog file = do {
        log <- decodeFile logf ;
        dic <- decodeFile dicf ;
        return (fromList dic,log)
    }  
    where
    basef = dropExtension file
    logf = addExtension basef "lox"
    dicf = addExtension basef "dic"

saveFullyDecompressedLog :: FilePath -> IO ()
saveFullyDecompressedLog path = 
  do compressThenSaveRawLog path
     (dic, clog) <- loadCompressedLog path
     writeFile newpath $ show $ fullyDecompress (dic, clog)
    where
      bname = takeBaseName path
      newpath = replaceBaseName path (bname ++ "_") 

test1 = do {
     (dict,log) <- compress logf ; 
      saveCompressedLog logf (dict,log) ;
      (dict2,log2) <-  loadCompressedLog logf ;
      return (dict==dict2, log==log2)       
   }    
   where
   logf = "./Eu/Fittest/Logging/Compression/SampleRawLog1.log"
   