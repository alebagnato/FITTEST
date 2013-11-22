{- 

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 

   This module provides ...
     
-}

module Eu.Fittest.Logging.Analysis.AppEventLog 

where

import Data.Maybe
import Data.List
import Eu.Fittest.Logging.Analysis.Base
import Eu.Fittest.Logging.XML.Base 
import Eu.Fittest.Logging.XML.AppEvent
import Eu.Fittest.Logging.XML.Value
import Eu.Fittest.Logging.XML.AppEventLog
import Eu.Fittest.Logging.Compression.RawlogParser
import Eu.Fittest.Logging.Compression.Compress
import Eu.Fittest.Logging.Compression.Serializer

getField_  fn o = find match (ofields o)
   where
   match (FieldValTy_  fx _ _) = fx == fn
   match (FieldXref_   fx _ )  = fx == fn
   match (FieldObj_    fx _ )  = fx == fn
   
getFieldx fn o = fromJust (getField_ fn o)

appEvent2EPS x = (e,p,s)
   where
   recordEvent = eEvent x
   e = (getFieldx "type" recordEvent, getFieldx "targetID" recordEvent) 
   p =  getFieldx "args" recordEvent
   s = eSmodel x
   
   
-- | Parse a sequence of app-events from a compressed log, and infer
--   its basic log profile. Note that this function assumes that
--   the log only contains app-events. Timestamps in the log should be
--   in compressed format.
--
-- profileEventLog :: Dictionary -> [CompressedLogEntry] -> [(Field_,Field_)]
profileEventLog dict log = ( 
                            0,
                            -- events_ stream,
                            -- eventsParams stream
                            -- readEvents stream,
                            -- assignmentLikeEvents stream,
                            indepEvents stream
                           )
    where
    stream = map appEvent2EPS events
    events = topRunCLparser parseAppEventLog dict log2
    log2   = decompressTimeStamp log   
    
test1 = do {
    (dict,log) <- loadCompressedLog "./Eu/Fittest/Logging/FlexStoreSample3.lox" ;
     return (profileEventLog dict log) 
 }    
