{- 

Author: Wishnu Prasetya

Copyright 2012 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 

   This module provides functionalities to encode control flows
   recoreded in a FITTEST log as a sequence of app events of
   the form f_sigma(args), where f is the function that generates
   some control flow p, and sigma is a subpath of p. The resulting
   app-event sequence is then translated to Daikon for oracle
   inference of the form predicate(args) ==> sigma.
     
-}

module Eu.Fittest.Logging.Daikon.LowLevelOracle.LLOBase(
     encodeNodeVisit,
     encodeSegment2Visit,
     showLLODaikonLog,
     saveAsLLODaikonLog
  )

where

import Debug.Trace
import Data.List
import System.IO
import System.FilePath
import Eu.Fittest.Logging.XML.Value
import Eu.Fittest.Logging.XML.Event
import Eu.Fittest.Logging.XML.AppEvent
import Eu.Fittest.Logging.XML.LowEvent
import Eu.Fittest.Logging.XML.EventLog
import Eu.Fittest.Logging.XML.Base
import Eu.Fittest.Logging.Compression.RawlogParser
import Eu.Fittest.Logging.Compression.Compress
import Eu.Fittest.Logging.Compression.Serializer
import Eu.Fittest.Logging.Daikon.DaikonConverter


type Path_  = [String]

-- Convert a list of (low and app) events to a list of
-- tuples (fe,fx,s) where fe represents a funtion-enter
-- fx is the corresponding exit, and s is the control
-- flow path that the function went through during that
-- call.
collectPaths :: [Event_] -> [(LowEvent_,LowEvent_,Path_)]
collectPaths log = collectPathsWorker [] log

collectPathsWorker stack [] = []
collectPathsWorker stack (AE _ : rest) = collectPathsWorker stack rest
collectPathsWorker stack (LE e : rest) 
   =
   case e of
      FE_  _ _ _ _ _  -> collectPathsWorker (e:stack) rest
      B_   _ _ _ _    -> collectPathsWorker (e:stack) rest
      FX_  _ _ _ _ _  -> 
           let
           (eOpen,path,stack2) = pop__ e stack
           in
           (eOpen,e,reverse path) : collectPathsWorker stack2 rest
                
      _       -> collectPathsWorker stack rest
   
-- A helper so to pop from an event-stack, so that pop__ fx stack   
-- will go through the stack's prefix z++[fe], where fe is the
-- matching function enter; then z would be the call's control flow
-- path. This prefix is popped out.
--
pop__ :: LowEvent_ -> [LowEvent_] -> (LowEvent_, Path_, [LowEvent_])
pop__ (FX_ _ fname cname _ _) stack = worker stack
   where
   worker [] = error "An FE_ event is not closed FX_."
   worker (B_ _ blockId _ _ : z) = (eOpen, blockId:path, stack2)
       where
       (eOpen,path,stack2) = worker z
   worker (eOpen@(FE_ _ fname' cname' _ _) : z) 
       =
       if (fname==fname' && cname==cname') then (eOpen, [], z)
       else error "An FE_ event is not closed by a matching FX_."
       
       
-- Encode a low level function entry/exit event-pair as an App-event,
-- whose name is extended with some sufix
--
encodeLow_ :: String -> LowEvent_ -> AppEvent_
encodeLow_ suffix (FE_ tstamp fname cname target args) 
   =
   AppEvent_ tstamp e smodel
       
   where
   smodel = null_
   null_  = SingleVal_ "null" "Null"  
   e      = Obj_ "eu.fittest.Logging::LAppEvent" fields
   fields = [ eXID, eTargetID, eType, eArgs ]
   eXID      =  FieldValTy_  "I" "10000" "ID"
   eTargetID =  FieldValTy_  "targetID" ("\"" ++ suffix ++ "\"") "String"
   eType     =  FieldValTy_  "type" ("\"" ++ cname ++ "." ++ fname ++ "\"") "String"
   eArgs     =  FieldObj_ "args" eArgs_
   eArgs_    =  Obj_ "Array" (aXID : [ mkArg a | a <- (target : args) ])
   aXID      =  FieldValTy_  "I" "20000" "ID"
   mkArg a   =  FieldObj_ "elem" a
   
{- |   
   Given a list z of events, this produces a new list z' of app events.
   For every every function call (f,p) in z, where p is the corresponding 
   control flow path, we put in z' app-events of the form f_i, where i is a 
   node-id passed by p. The idea is to use the Daikon converter to infer
   oracles from this.
-}
encodeNodeVisit :: [Event_] -> [Event_]
encodeNodeVisit eventSequence = concat [ encode p | p <- paths ]
   where
   paths = collectPaths eventSequence
   encode (eOpen,eClose,path) = [ AE (encodeLow_ nodeId eOpen) | nodeId <- path ]
  
{- |  
   Analogous to encodeNodeVisit, but this encodes every subpath [i,j] 
   in p as an app-event f_i_j.
-}
encodeSegment2Visit :: [Event_] -> [Event_] 
encodeSegment2Visit eventSequence = concat [ encode p | p <- paths ]
   where
   paths = collectPaths eventSequence
   encode (eOpen,eClose,path) = [ AE (encodeLow_ idid eOpen) | idid <- z ]
      where
      z = zipWith (\x y -> x ++ "_" ++ y) path (tail path)
   

   
   
{- | This reads a FITTEST log to produce a sequence of app events
     encoding the recorded conrol flows in the log. The sequence
     is then translated to the Daikon format, and saved.
     The string is
     the name/path to the FITTEST log. It consumes a compressed
     log, not the raw log (so you must have covert the .log file
     to the pair .lox and .dic). The function creates .dtrace file.
-}        
saveAsLLODaikonLog :: Int -> String -> IO()
saveAsLLODaikonLog strength file = do {
    (dict,log1) <- loadCompressedLog file ;
    let
    log2      = decompressTimeStamp log1
    events    = uncurry (topRunCLparser parseEventLog) (dict,log2) 
    daikonLog = showLLODaikonLog strength events
    basef     = dropExtension file
    daikonf   = addExtension basef "dtrace"
    in 
    -- putStr daikonLog
    writeFile daikonf daikonLog
   }

{- | This converts a sequence of events to a Daikon log (in the 
     resulting string). The int is used to select which encoding
     is used. Currently, int 1 or less will map to the NodeVisit
     encoder, else we use Segment2Visit encoder.
-}   
showLLODaikonLog :: Int -> [Event_] -> String
showLLODaikonLog strength events = showAsDaikonLog . encoder $ events
   where
   encoder = if strength<=1 then encodeNodeVisit
                     else encodeSegment2Visit
        
   
   
---  ==========================================
---  Example
---

mkStructuredLog rawlog 
   = 
   uncurry (topRunCLparser parseEventLog) (dic1,log2) 
   where
   log2 = decompressTimeStamp log1
   (dic1,log1) = strCompress rawlog 

exDLog0   = putStr . showAsDaikonLog . encodeSegment2Visit $ exLog0_
exLLOLog0 = putStr . show . encodeSegment2Visit $ exLog0_
exLog0_   = mkStructuredLog exLog0__   
exLog0__  = ex0  
         
-- example of two nested calls         
ex0 = concat . intersperse "\n" $
    [ "%<S -60:1331487654450 \"FE:Update:Game\"",
      "%<S \"O:Game\"",
      "    %<P",
      "    %<{ I=0:ID }%>",
      "    %<{ gameover=false:Boolean }%>",
      "    %>",
      "%>",
      "%<S \"args\"",
      "   %<P %<{ 10:int }%> %>",
      "   %<P %<{ 99:int }%> %>",
      "%>",
      "%>",
      
      "%<S -60:1331487654450 \"B:562:Update:Game\" %>",
      "%<S -60:1331487654450 \"B:630:Update:Game\" %>",
      -- nested call:
      "%<S -60:1331487654450 \"FE:Update:Game\"",
      "%<S \"O:Game\"",
      "    %<P",
      "    %<{ I=0:ID }%>",
      "    %<{ gameover=false:Boolean }%>",
      "    %>",
      "%>",
      "%<S \"args\"",
      "   %<P %<{ 10:int }%> %>",
      "   %<P %<{ 99:int }%> %>",
      "%>",
      "%>",
      
      "%<S -60:1331487654450 \"B:562:Update:Game\" %>",
      "%<S -60:1331487654450 \"B:563:Update:Game\" %>",
       
      "%<S -60:1331487654450 \"FX:Update:Game\"",
      "%<S \"O:Game\"",
      "    %<P",
      "    %<{ I=0:ID }%>",
      "    %<{ gameover=false:Boolean }%>",
      "    %>",
      "%>",
      "%<P %<{ undefined:void }%> %>",
      "%>",
      -- back to parent call
      "%<S -60:1331487654450 \"B:631:Update:Game\" %>",
      "%<S -60:1331487654450 \"B:640:Update:Game\" %>",

      "%<S -60:1331487654450 \"FX:Update:Game\"",
      "%<S \"O:Game\"",
      "    %<P",
      "    %<{ I=0:ID }%>",
      "    %<{ gameover=false:Boolean }%>",
      "    %>",
      "%>",
      "%<P %<{ undefined:void }%> %>",
      "%>"
    ]
