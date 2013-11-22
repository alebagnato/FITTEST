{-

Author: Wishnu Prasetya

Copyright 2013 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- |

  This module provides functions to pretty-print internal
  representation of FITTEST raw logs, back to textual raw
  logs.
  
  No indentation is added to save space.
-}

module Eu.Fittest.Logging.Compression.RawlogPrettyPrinter(
     ppRawLog
  )

where

import Eu.Fittest.Logging.Compression.RawlogParser
import Codec.Binary.UTF8.String
import qualified Data.ByteString.Lazy as B
import Data.Text.IO

ppRawLog :: [RawLogEntry UTCTimeStamp1970 String [String]] -> B.ByteString
ppRawLog = ppEntries 

ppEntries :: [RawLogEntry UTCTimeStamp1970 String [String]] -> B.ByteString
ppEntries entries = foldr B.append B.empty [ ppEntry e | e <- entries ]

packString s = B.pack . encode $ s

ppEntry :: RawLogEntry UTCTimeStamp1970 String [String] -> B.ByteString
ppEntry (Section t attrib entries)
    = 
    packString ("%<S " ++ timeStamp ++ attrib ++ "\n")
    `B.append`
    ppEntries entries
    `B.append`
    packString "%>\n"
    where
    timeStamp = case t of 
                Just (offset,elapseTime) -> show offset ++ ":" ++ show elapseTime ++ " "
                _   ->  ""
                
ppEntry (Par p) = ppParagraph p

ppParagraph :: [String] -> B.ByteString
ppParagraph sentences =  packString "%<P\n"
                         `B.append` ppSentences sentences
                         `B.append` packString "%>\n" 
                        

ppSentences sentences = foldr B.append B.empty [ppSentence s | s <- sentences ]

ppSentence :: String -> B.ByteString
ppSentence s = packString ("%<{ " ++ s ++ " }%>\n")                      
              
--
-- test examples:
--    
    
exLog1 = 
    do 
      log <- Data.Text.IO.readFile "./SampleRawLog1.log" 
      let parsedLog = strParseRawLog log
          z = ppRawLog parsedLog
      B.putStr z 
   
