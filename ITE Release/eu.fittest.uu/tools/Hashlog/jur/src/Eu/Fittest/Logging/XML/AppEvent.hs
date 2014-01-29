{- 

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 

   This module provides a type representing application-events which
   are to be read from a log, and:

   * a parser to read from semi-compressed raw log.
   * XML formater.   
     
-}

module Eu.Fittest.Logging.XML.AppEvent(
     AppEvent_ (..),
     parseAppEvent,
     getAppEventName,
     getAppEventTargetID,
     getAppEventArgs
  )

where

import Debug.Trace
import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.IntMap
import Data.List
import Text.XML.Light
import Eu.Fittest.Logging.XML.Base
import Eu.Fittest.Logging.XML.Value
import Eu.Fittest.Logging.Compression.RawlogParser
import Eu.Fittest.Logging.Compression.Compress

-- | Type representing application events in the log.
data AppEvent_ =
        -- | Application event 
        AppEvent_  { 
                     -- | Time stamp.
                     aeStamp  :: Maybe UTCTimeStamp1970,
                     -- | Typically contain event's name and parameters.
                     aeEvent  :: Value_,
                     -- | Represent application state after the event.
                     aeSmodel :: Value_ 
                 }
        -- | Entry representing application state. Typically the initial state.
        | AppState_ {
            aeStamp :: Maybe UTCTimeStamp1970,
            aeState :: Value_
        }
        deriving (Show,Eq)
                 
{- ==============================================================
   The parser part
============================================================== -}
   
-- | To parse application-event from semi-compressed raw log.   
parseAppEvent :: CompressedLogParser AppEvent_ 
parseAppEvent  = do {
      dict <- getDict_ ;
      s@(Section ts tag_ elems) <- satisfy_ isSection `elseError` "Parsing an application event. Expecting a section";
      case getTag_ dict tag_ of
        Just "E"  ->  parseAppEvent_  s
        Just "S"  ->  parseAppState_  s
        Just _    ->  fail "Parsing an application event. The tag is wrong." 
                           `withEvidence` [s]
        Nothing   ->  fail "Parsing an application event. Tag could not be found in dict"
                           `withEvidence` [s]
   }            

getTag_ dict (Tag_ t)  = Just t
getTag_ dict (XTag_ i) = do {
                            t <- Data.IntMap.lookup i dict ;
                            if isSing t then return (head t) else fail ""
                            }   
            
isSing [x] = True
isSing  _  = False 
            
parseAppEvent_ :: SemiCompressedLogEntry -> CompressedLogParser AppEvent_
parseAppEvent_  s@(Section ts tag_ elems) = do {
   dict <- getDict_ ;
   if not(length elems == 2) 
   then fail "Parsing an app-event. Error in its number of elements"
             `withEvidence` [s]
             
   else (case runCLparser (all_ parseValue) dict elems of
           Right ([event,state],[])  -> 
              return (AppEvent_ { aeStamp = ts, 
                                  aeEvent=event, 
                                  aeSmodel=state })
           _  -> fail "Parsing an app-event. Failing on its elements"
                      `withEvidence` [s]
        )
   }              
   
parseAppState_ :: SemiCompressedLogEntry -> CompressedLogParser AppEvent_
parseAppState_  s@(Section ts tag_ elems) = do {
   dict <- getDict_ ;
   if not(length elems == 1) 
   then fail "Parsing an app-state. Error in its number of elements"
             `withEvidence` [s]
             
   else (case runCLparser (all_ parseValue) dict elems of
           Right ([state],[])  -> 
              return (AppState_ { aeStamp = ts, 
                                  aeState = state })
           _  -> fail "Parsing an app-state. Failing on its elements"
                      `withEvidence` [s]
        )
   }    
   
     
{- ==============================================================
   The XML formater part
============================================================== -}

instance ConvertibleToXML AppEvent_ where

   toXML (AppEvent_  { aeStamp = ts_ , aeEvent=e, aeSmodel=s })
       =
       mkElem "E" attrs [ toXML e, toXML s ]
       where
       attrs = case ts_ of
                  Nothing          -> []
                  Just (offset,t)  -> [attrib1 "t" (show offset ++ ":" ++ show t) ]
                  
   toXML (AppState_ { aeStamp = ts_ , aeState = s })
       = 
       mkElem "S" attrs [ toXML s ]
       where
       attrs = case ts_ of
                  Nothing          -> []
                  Just (offset,t)  -> [attrib1 "t" (show offset ++ ":" ++ show t) ]

                  
{- ==============================================================
   Utility functions
============================================================== -}
      
-- | Return the (string) name of the application event.      
getAppEventName :: AppEvent_ -> Maybe String         
getAppEventName e = if isNull name_ || isUndefined name_ || (getValType name_ /= "String") then Nothing
                    else Just . read . vVal $ name_
   where
   e_ = aeEvent e
   xrefmap  = buildXREFmap e_
   name_ = getField__ xrefmap "type" e_

-- | Return the (string) ID of the object targetted by the application event.         
getAppEventTargetID :: AppEvent_ -> Maybe String   
getAppEventTargetID e = if isNull id_ || isUndefined id_ || (getValType id_ /= "String") then Nothing
                        else Just . read . vVal $ id_ 
   where
   e_ = aeEvent e
   xrefmap  = buildXREFmap e_
   id_ = getField__ xrefmap "targetID" e_
   
-- | Return the list of the arguments which the application event took.
--  
getAppEventArgs :: AppEvent_ -> [Value_]   
getAppEventArgs e = args
   where
   e_ = aeEvent e
   xrefmap  = buildXREFmap e_  
   args     = getDeepAllField xrefmap ["args","elem"] e_ 
   
                  
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

exAppEvlog0_  = testLowLevelParser_  (all_ parseAppEvent)  exAppEvlog0__
exAppEvlog0__ = internalRepRawLog__  exAppEvlog0
 
exAppEvlog0 = concat . intersperse "\n" $
        ["%<S \"S\" %<P %<{ \"some value\":String }%> %> %>"]
        ++ 
        ["%<S -120:1312489705920 \"E\"",
         -- the event object
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
         -- the app state object 
         "%<S \"O:mx.controls::TextInput\"",
         "%<P",
         "%<{ I=0:ID }%>",
         "%<{ id=\"r\":String }%>",
         "%<{ text=\"0\":String }%>",
         "%>",
         "%>",
         -- end of the top E-section
         "%>"
        ]         