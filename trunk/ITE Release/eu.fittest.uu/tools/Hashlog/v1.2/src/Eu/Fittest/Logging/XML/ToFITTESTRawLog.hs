{- 

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 

   This module provides a coverter from the Internal representation
   of structured FITTEST log to raw FITTEST law.
   
-}

module Eu.Fittest.Logging.XML.ToFITTESTRawLog(
     events2raw
   , events2raw_bytestring
   )

where

import Eu.Fittest.Logging.Compression.RawlogParser
import Eu.Fittest.Logging.XML.Value
import Eu.Fittest.Logging.XML.LowEvent
import Eu.Fittest.Logging.XML.AppEvent
import Eu.Fittest.Logging.XML.Event
import Eu.Fittest.Logging.Compression.RawlogPrettyPrinter
import qualified Data.ByteString.Lazy as B


-- field2raw f = ( sentence, Maybe rawlogEntry)
field2raw :: Field_ -> (String, Maybe (RawLogEntry UTCTimeStamp1970 String [String]))
field2raw (FieldValTy_  name val ty) = (name ++ "=" ++ val ++ ":" ++ ty, Nothing)
field2raw (FieldXref_   name ref) = (name ++ "=^" ++ show ref, Nothing)
field2raw (FieldObj_ name obj) = (name ++ "=>", Just (obj2raw obj))

-- convert Obj to raw
obj2raw :: Value_ -> RawLogEntry UTCTimeStamp1970 String [String]
obj2raw v@(SingleVal_ _ _)  = error ("obj2raw expects an object, but it gets a simple value " ++ show v)
obj2raw (Obj_ cname fields) = Section Nothing attrib subsections
    where
    attrib = show ("O:" ++ cname)
    subsections = group (map field2raw fields)
    group [] = []
    group fields_ = [Par (reverse sentences)] ++ o_ ++ group rest
       where
       (sentences,o,rest) = split [] fields_ 
       o_ = case o of 
              Just o' -> [o']
              _       -> []
    
    -- split the fields to get the maximum group of simple sentences 
    -- until we get to the first field that points to an object 
    split paragraph [] = (paragraph, Nothing, [])
    split paragraph ((s, Just o) : rest) = (s:paragraph, Just o, rest)
    split paragraph ((s, _) : rest)      = split (s:paragraph) rest
    
-- Value to Raw    
val2raw :: Value_ -> RawLogEntry UTCTimeStamp1970 String [String]
val2raw (SingleVal_ val ty) = Par [val ++ ":" ++ ty]
val2raw o@(Obj_ _ _) = obj2raw o

-- Converting low event to Raw

wrapArgs args = Section Nothing "args" [val2raw v | v<-args]
wrapCaller callerName callerClass = Par [ callerName ++ ":" ++ callerClass ]

lowEvent2raw (FE_ tstamp fname cname target args) 
   =
   Section tstamp (show ("FE:" ++ fname ++ ":" ++ cname)) [val2raw target, wrapArgs args]
   
lowEvent2raw (FX_ tstamp fname cname target retVal) 
   =
   Section tstamp (show ("FX:" ++ fname ++ ":" ++ cname)) [val2raw target, val2raw retVal]

lowEvent2raw (FCE_ tstamp fname cname callerName callerClass target args) 
   =
   Section tstamp (show ("FCE:" ++ fname ++ ":" ++ cname)) 
           [ wrapCaller callerName callerClass,
             val2raw target, 
             wrapArgs args ]
   
lowEvent2raw (FCX_ tstamp fname cname callerName callerClass target retVal exception) 
   =
   Section tstamp (show ("FCE:" ++ fname ++ ":" ++ cname))
           [ wrapCaller callerName callerClass,
             val2raw target, 
             val2raw retVal,
             val2raw exception ]   
             
lowEvent2raw (B_ tstamp bid fname cname) 
   =
   Section tstamp (show ("B:" ++ bid ++ ":" ++ fname ++ ":" ++ cname)) [] 
 
lowEvent2raw (BEH_ tstamp bid fname cname exception) 
   =
   Section tstamp (show ("BEH:" ++ bid ++ ":" ++ fname ++ ":" ++ cname)) [val2raw exception]

lowEvent2raw (BLE_ tstamp bid fname cname) 
   =
   Section tstamp (show ("BLE:" ++ bid ++ ":" ++ fname ++ ":" ++ cname)) [] 
 
lowEvent2raw e@(BLX_ tstamp bid fname cname cnt) 
   =
   Section tstamp (show ("BLX:" ++ bid ++ ":" ++ fname ++ ":" ++ cname)) cnt'
   where
   cnt' = case cnt of
            Just c -> [Par ["cnt=" ++ show c]]
            _      -> []      

-- Converting app-event to Raw

appEvent2raw (AppEvent_  tstamp event state) 
   =
   Section tstamp (show "E") [val2raw event, val2raw state]
 
appEvent2raw (AppState_  tstamp state) = Section tstamp (show "S") [val2raw state]

-- Top level
event2raw (AE e) = appEvent2raw e
event2raw (LE e) = lowEvent2raw e

events2raw events = [event2raw e | e <- events]

events2raw_bytestring :: [Event_] -> B.ByteString
events2raw_bytestring events = ppRawLog . events2raw $ events

 