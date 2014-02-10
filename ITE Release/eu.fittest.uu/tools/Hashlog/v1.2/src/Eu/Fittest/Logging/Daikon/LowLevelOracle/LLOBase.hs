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
     showAsLLODaikonLog,
     saveAsLLODaikonLog,
     LLOOptions(..)
  )

where

import Debug.Trace
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Text.RegexPR
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
type PathTuple_ = (LowEvent_,   -- FE
                   LowEvent_,   -- FX
                   Path_ )

-- Convert a list of (low and app) events to a list of
-- tuples (fe,fx,s) where fe represents a funtion-enter
-- fx is the corresponding exit, and s is the control
-- flow path that the function went through during that
-- call.
collectPaths :: [Event_] -> [PathTuple_]
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
  
  
-- Filter a list of path-tuples based on a regular expression, and group them
-- according to their function-names.
--   
filterAndGroup_ :: String   -- ^ A regular expression to select which functions to be included 
                   -> [PathTuple_]
                   -> Map.Map String [PathTuple_]
filterAndGroup_ regex ptps = worker ptps
   where
   worker [] = Map.empty
   worker (f@(eOpen,_,_) : rest) = case matchRegexPR regex name of
      Nothing  -> pmap
      Just _   -> if Map.member name pmap 
                     then Map.adjust (\z-> f : z) name pmap
                     else Map.insert name [f] pmap
      where
      pmap  = worker rest
      name  = leCname eOpen ++ "." ++ leFname eOpen
   
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
-- whose name is extended with some sufix.
-- In this encoding, the final values of the function args (so their
-- value at the exit) are ignored. --> this is a restriction coming
-- from the way high-level events are treated. Perhaps we should change
-- that. Else we can still represent this by encoding them as part of
-- the return value.
--
-- BUT, the return value is for now ignored. --> FIX ME!
--
encodeLow_ :: String -> LowEvent_ -> LowEvent_ -> [Event_]
encodeLow_ suffix (FE_ tstamp1 fname cname target1 args) 
                  (FX_ tstamp2 _ _ target2 ret) 
   =
   
   [AE (AppEvent_ tstamp1 dummyAppEvent_ target1), AE (AppEvent_ tstamp2 e target2) ]
       
   where
   -- null_  = SingleVal_ "null" "Null"  
   e      = Obj_ appEventTy_ fields
   fields = [ eXID, eTargetID, eType, eArgs ]  
   eTargetID =  FieldValTy_  "targetID" ("\"" ++ suffix ++ "\"") "String"
   eType     =  FieldValTy_  "type" ("\"" ++ cname ++ "." ++ fname ++ "\"") "String"
   eArgs     =  FieldObj_ "args" eArgs_
   eArgs_    =  Obj_ "Array" (aXID : [ mkArg a | a <- args])
   
   mkArg a   =  FieldObj_ "elem" a
      
appEventTy_ = "eu.fittest.Logging::LAppEvent"
eXID      =  FieldValTy_  "I" "10000" "ID"
aXID      =  FieldValTy_  "I" "20000" "ID"

dummyAppEvent_ =  Obj_ appEventTy_  [ eXID, eTargetID, eType, FieldObj_ "args" eArgs_]
   where
   eTargetID =  FieldValTy_  "targetID" "\"dummy\"" "String"
   eType     =  FieldValTy_  "type" "\"__dummy\"" "String"
   eArgs_    =  Obj_ "Array" []

   
{- |   
   Given a list ps of path-tuples, this produces a new list z' of app events.
   For tuple (fe,fx,p) in ps, where p is the corresponding 
   control flow path, we put in z' app-events of the form f_i, where i is a 
   node-id passed by p. The idea is to use the Daikon converter to infer
   oracles from this.
-}
encodeNodeVisit :: [PathTuple_] -> [Event_]
encodeNodeVisit pathTuples = concat [ encode p | p <- pathTuples ]
   where
   encode (eOpen,eClose,path) 
     = 
     -- Throwing away the entry node:
     -- concat [ encodeLow_ nodeId eOpen eClose | nodeId <- nub (strip1_ path) ]
     -- Entry node is not thrown:
     concat [ encodeLow_ nodeId eOpen eClose | nodeId <- nub path ]
  
-- stripping the entry node, unless it is the only node:
strip1_ []  = []
strip1_ [n] = [n]
strip1_  (_:rest) = rest
  
{- |  
   Analogous to encodeNodeVisit, but this encodes every subpath [i,j] 
   in p as an app-event f_i_j. Essentially, edge rather than node.
-}
encodeSegment2Visit :: [PathTuple_] -> [Event_]
encodeSegment2Visit pathTuples = concat [ encode p | p <- pathTuples ]
   where
   encode (eOpen,eClose,path) 
      = 
      concat [ encodeLow_ idid eOpen eClose | idid <- nub z ]
      where
      z = zipWith (\x y -> x ++ "_" ++ y) path (tail path)

      
encodePathVisit :: [PathTuple_] -> [Event_]
encodePathVisit pathTuples = concat [ encode p | p <- pathTuples ]
   where
   encode (eOpen,eClose,path) 
     = 
     encodeLow_ (mkPathName (nub path)) eOpen eClose
   
mkPathName nodes = concat (intersperse "_" nodes)

-- | Representing options for LLO conversion   
data LLOOptions =  LLOOptions {
      lloOptRegexFunctionsChoice :: String,  -- ^ Regex specifying which functions to be included
      lloOptMode :: Int,                     -- ^ Conversion mode (node/edges/path)
      lloChopAt  :: Maybe Int          -- ^ Maximum number of paths considered
   }


llo_ :: LLOOptions -> [Event_] -> Map.Map String [Event_]
llo_ opts events = Map.map encoder pmap
   where
   pathTuples1 = collectPaths events
   pathTuples2 = case lloChopAt opts of
                    Just k -> take k pathTuples1
                    _      -> pathTuples1                    
   
   pmap =  filterAndGroup_ (lloOptRegexFunctionsChoice opts) pathTuples2
   
   encoder = case lloOptMode opts of
      1 -> encodeNodeVisit 
      2 -> encodeSegment2Visit   
      3 -> encodePathVisit 
      _ -> encodeNodeVisit 
   

{- | This reads a FITTEST log to produce a sequence of app events
     encoding the recorded conrol flows in the log. The sequence
     is then translated to the Daikon format, and saved.
     The string is
     the name/path to the FITTEST log. It consumes a compressed
     log, not the raw log (so you must have covert the .log file
     to the pair .lox and .dic). The function creates .dtrace file.
-}        
saveAsLLODaikonLog :: Bool  -- -- ^ flag to/not-to print statistics (may cause memory spike!)   
                      -> DaikonConverterOptions  -- ^ Daikon options
                      -> LLOOptions              -- ^ LLO options
                      -> String                  -- ^ Name of the source (compressed) logfile
                      -> String                  -- ^ Name of output file; if empty then <source>.dtrace
                      -> IO()
saveAsLLODaikonLog printStatsFlag daikonOptions lloOptions file outf
   = 
   do {
    (dict,log1) <- loadCompressedLog file ;
    let
    log2      = decompressTimeStamp log1
    events    = uncurry (topRunCLparser parseEventLog) (dict,log2) 
    groups    = Map.assocs (llo_ lloOptions events)
    in
    do {
      sequence_ [toDaikon z | z <- groups ] ;
      putStrLn ("\n** LLO done. Generating Daikon-logs of " ++ show (length groups) ++ " functions.") 
      }
   }
   where
   basef  = if null outf then dropExtension file
                         else dropExtension outf

   rename name = [ if c == ':' then '.' else c | c<-name]
   toDaikon (name,events) = do {
         putStrLn "----" ;
         saveAsDaikonLog_ daikonConverter printStatsFlag daikonOptions outfile events ;
       }
       where
       outfile = basef ++ "_" ++ rename name ++ ".dtrace"



 


showAsLLODaikonLog :: DaikonConverterOptions -> LLOOptions -> [Event_] -> String
showAsLLODaikonLog daikonOptions lloOptions events 
   = 
   concat . intersperse "\n-----\n" $ results
   
   where
   groups  = Map.assocs (llo_ lloOptions events)
   mkHeader n = "** " ++ n ++ ":\n"
   results = [ mkHeader n ++ showAsDaikonLog daikonOptions z | (n,z) <- groups ] 
 
   
   
---  ==========================================
---  Example
---

mkStructuredLog rawlog 
   = 
   uncurry (topRunCLparser parseEventLog) (dic1,log2) 
   where
   log2 = decompressTimeStamp log1
   (dic1,log1) = strCompress rawlog 

exDLog0   = putStr . showAsLLODaikonLog daikonOptions lloOptions $ exLog0_
   where
   lloOptions = LLOOptions {
     lloOptRegexFunctionsChoice = "Update",
     lloOptMode = 1 ,
     lloChopAt = Nothing
   }
   daikonOptions = DConvOptions {
       dcOptGenerateAuxVars = False,
       dcOptRegexVarsChoice = "",
       dcOptRegexAppEventsChoice = ""
    }
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
