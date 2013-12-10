{- 

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 

   This module provides a type representing both high level and low level 
   events which are to be read from a log, and:

   * a parser to read from semi-compressed raw log.
   * XML formater.   
     
-}

module Eu.Fittest.Logging.XML.Event(
      Event_(..),
      parseEvent
   )

where

import Text.ParserCombinators.ReadP
import Text.XML.Light
import Eu.Fittest.Logging.XML.Base
import Eu.Fittest.Logging.XML.Value
import Eu.Fittest.Logging.XML.AppEvent
import Eu.Fittest.Logging.XML.LowEvent
import Eu.Fittest.Logging.Compression.RawlogParser
import Eu.Fittest.Logging.Compression.Compress
import Data.IntMap
import Data.List

-- | Type representing  events in the log.
data Event_ = AE AppEvent_ | LE LowEvent_ deriving (Show,Eq)

-- | To parse an event from semi-compressed raw log.
parseEvent :: CompressedLogParser Event_
parseEvent =  do { e <- parseLowEvent ; return $ LE e }
              <<| 
              do { e <- parseAppEvent ; return $ AE e }
                     
instance ConvertibleToXML Event_ where
   toXML (AE e) = toXML e
   toXML (LE e) = toXML e
   
{- ==============================================================
   Examples for test
   
   use e.g. 
     uncurry (topRunCLparser parser) example  to test parsers
     putStr . ppElement . toXML . uncurry (topRunCLparser parser) $ example  to test XML converter
============================================================== -}  

testLowLevelParser_  parser log = 
    sequence_
    . Data.List.map (putStr . ppTopElement) 
    . Data.List.map toXML 
    . uncurry (topRunCLparser parser)
    $ log

internalRepRawLog__ rawlog = (dic1,log2) 
   where
   log2 = decompressTimeStamp log1
   (dic1,log1) = strCompress rawlog 
   
exLog1  = testLowLevelParser_  (all_ parseEvent)  exLog1_ 
exLog1_ = internalRepRawLog__ exLog1__
   
exLog1__  =  concat . intersperse "\n" $
        ["%<S -120:1318674228273 \"B:2177:recGcd:GCDplain\" %>",
         "%<S -120:1318674228273 \"B:2178:recGcd:GCDplain\" %>",
         "%<S \"B:2182:recGcd:GCDplain\" %>"
        ]
        ++
        ["%<S -120:1312489705920 \"E\"",
         "%<S \"O:mx.controls::TextInput\"",
         "%<P",
         "%<{ I=0:ID }%>",
         "%<{ id=\"r\":String }%>",
         "%<{ text=\"0\":String }%>",
         "%>",
         "%>",
         "%<S \"O:eu.fittest.actionscript.automation::RecordEvent\"",
         "%<P",
         "%<{ I=0:ID }%>",
         "%<{ targetID=\"Calculate\":String }%>",
         "%<{ type=\"click\":String }%>",
         "%<{ args=> }%>",
         "%>",
         "%<S \"O:Array\"",
         "%<P",
         "%<{ I=1:ID }%>",
         "%>",
         "%>",
         "%>",
         "%>"
        ]   