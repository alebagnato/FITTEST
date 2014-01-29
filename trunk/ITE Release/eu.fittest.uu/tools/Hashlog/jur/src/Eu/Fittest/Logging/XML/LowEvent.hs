{- 

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 

   This module provides a type representing low level events which
   are to be read from a log, and:

   * a parser to read from semi-compressed raw log.
   * XML formater.   
     
-}

module Eu.Fittest.Logging.XML.LowEvent(
      LowEvent_(..),
      parseLowEvent
   )

where

import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.IntMap
import Data.List
import Text.XML.Light
import Eu.Fittest.Logging.XML.Base
import Eu.Fittest.Logging.XML.Value
import Eu.Fittest.Logging.Compression.RawlogParser
import Eu.Fittest.Logging.Compression.Compress

-- | Type representing low level events in the log.
data LowEvent_ = FE_ { 
                    leStamp  :: Maybe UTCTimeStamp1970,
                    leFname  :: String,
                    leCname  :: String,
                    leTarget :: Value_,
                    leArgs   :: [Value_]
                 }
        |  FX_ { 
                    leStamp  :: Maybe UTCTimeStamp1970,
                    leFname  :: String,
                    leCname  :: String,
                    leTarget :: Value_,
                    leReturn :: Value_
                 }
        | FCE_ { 
                    leStamp  :: Maybe UTCTimeStamp1970,
                    leFname  :: String,
                    leCname  :: String,
                    leCallerName   :: String,
                    leCallerCName  :: String,
                    leTarget :: Value_,
                    leArgs   :: [Value_]
                 }
        |  FCX_ { 
                    leStamp  :: Maybe UTCTimeStamp1970,
                    leFname  :: String,
                    leCname  :: String,
                    leCallerName   :: String,
                    leCallerCName  :: String,
                    leTarget :: Value_,
                    leReturn :: Value_,
                    leException :: Value_
                 }         
        |  B_ { 
                    leStamp  :: Maybe UTCTimeStamp1970,
                    leBId     :: String,
                    leFname  :: String,
                    leCname  :: String
                 }  
        |  BEH_ { 
                    leStamp  :: Maybe UTCTimeStamp1970,
                    leBId     :: String,
                    leFname  :: String,
                    leCname  :: String,
                    leException  :: Value_
                 }  
        |  BLE_ { 
                    leStamp  :: Maybe UTCTimeStamp1970,
                    leBId     :: String,
                    leFname  :: String,
                    leCname  :: String
                 }   
        |  BLX_ { 
                    leStamp  :: Maybe UTCTimeStamp1970,
                    leBId     :: String,
                    leFname  :: String,
                    leCname  :: String,
                    leCnt    :: Maybe Integer
                 }                  
        deriving (Show,Eq)        

{- ==============================================================
   The parser part
   
   Lots of duplication. Consolidation is needed.
============================================================== -}

-- | To parse low-level event from semi-compressed raw log.
parseLowEvent :: CompressedLogParser LowEvent_ 
parseLowEvent = parseLowEvent_

parseLowEvent_ :: CompressedLogParser LowEvent_ 
parseLowEvent_  = do {
      dict <- getDict_ ;
      s@(Section ts tag_ elems) <- satisfy_ isSection `elseError` "Parsing a low level event. Expecting a section";
      case getTag_ dict tag_ of
        Just tag  ->  parseLowEvent__  tag s
        Nothing   ->  fail "Parsing a low level event. Tag could not be found in dict"
                     `withEvidence` [s]
   }

parseLowEvent__ :: String -> SemiCompressedLogEntry -> CompressedLogParser LowEvent_ 
parseLowEvent__  tag s =  case tagHead of
   "FE"  -> parseFE_     tagTail s
   "FX"  -> parseFX_     tagTail s
   "B"   -> parseBlock_  tagTail s
   "BEH" -> parseBEH_    tagTail s
   "BLE" -> parseBLE_    tagTail s
   "BLX" -> parseBLX_    tagTail s
   "FCE" -> parseFCE_    tagTail s
   "FCX" -> parseFCX_    tagTail s
   _     -> fail "Parsing a low level event. Unknown tag." `withEvidence` [s]
   where
   (tagHead,tagTail_) = span (/= ':') tag 
   tagTail            = drop 1 tagTail_  

getTag_ dict (Tag_ t)  = Just t
getTag_ dict (XTag_ i) = do {
                            t <- Data.IntMap.lookup i dict ;
                            if isSing t then return (head t) else fail ""
                            }
                            
isSing [x] = True
isSing  _  = False                              
                            
getSentences_  dict (Par_ sentences)  = Just sentences
getSentences_ dict (XPar_ i) = do {
                            z <- Data.IntMap.lookup i dict ;
                            return z
                            }                            
                            
parseFE_ :: String -> SemiCompressedLogEntry -> CompressedLogParser LowEvent_      
parseFE_  tagTail s@(Section ts _ elems) = 
    let 
    (fn,cn) = splitFNCN tagTail
    in
    if not(length elems == 2)
    then fail "Parsing an FE event. Error in its number of elements." `withEvidence` [s]
    else do {
      dict <- getDict_ ;
      (case runCLparser parseTargetAndArgs_ dict elems of
          Right ((target,args),[]) -> 
            return (FE_ { 
              leStamp = ts,
              leFname = fn,
              leCname = cn,
              leTarget = target,
              leArgs   = args })
          _  -> fail "Parsing an FE event. Error in parsing its target or arguments."
                        `withEvidence` [s]
      )
    }        

splitFNCN tagTail = (fn, drop 1 cn_)
   where
   (fn,cn_) =  span (/= ':') tagTail   
   
splitBidFNCN tagTail = (bid, fn, cn)
   where
   (bid,fnamePart) = span (/= ':') tagTail 
   (fn,cn)         = splitFNCN fnamePart

parseTargetAndArgs_ :: CompressedLogParser (Value_, [Value_])
parseTargetAndArgs_ = do {
  t <- parseValue ;
  args <- parseArgs_ ;
  return (t,args)
  }
   
parseArgs_ :: CompressedLogParser [Value_]
parseArgs_  = do {
      dict <- getDict_ ;
      s@(Section _ tag_ elems) <- satisfy_ isSection `elseError` "Parsing function arguments. Expecting a section";
      case getTag_ dict tag_ of
         Nothing  -> fail "Parsing function arguments. Tag could not be found in dict"
                     `withEvidence` [s]
         Just tag -> if not(isArgsTag tag)
                     then fail "Parsing function arguments. Error in its tag." `withEvidence` [s]
                     else
                     (case runCLparser (all_ parseValue) dict elems of
                        Right (args,[]) ->  return args     
                        _    ->    fail "Parsing function arguments. Error in parsing them."
                        `withEvidence` [s]
                     )
   }            
   where                     
   isArgsTag tag = (take 4 tag) == "args"

parseFX_ :: String -> SemiCompressedLogEntry -> CompressedLogParser LowEvent_      
parseFX_  tagTail s@(Section ts _ elems) = 
    let 
    (fn,cn) = splitFNCN tagTail
    in
    if not(length elems == 2)
    then fail "Parsing an FX event. Error in its number of elements." `withEvidence` [s]
    else do {
      dict <- getDict_ ;
      (case runCLparser (all_ parseValue) dict elems of
          Right ([target,ret],[])
             -> return (FX_ { 
                        leStamp = ts,
                        leFname = fn,
                        leCname = cn,
                        leTarget = target,
                        leReturn = ret
                        })
                        
          _  -> fail "Parsing an FX event. Error in parsing its target or return value." 
                     `withEvidence` [s]
      )
    }        

    
parseFCE_ :: String -> SemiCompressedLogEntry -> CompressedLogParser LowEvent_      
parseFCE_  tagTail s@(Section ts _ elems) = 
    let 
    (fn,cn) = splitFNCN tagTail
    in
    if not(length elems == 3)
    then fail "Parsing an FCE event. Error in its number of elements." `withEvidence` [s]
    else do {
      dict <- getDict_ ;
      (case runCLparser parseCaller dict [elems!!0] of
           Right ((crFn,crCn),[]) ->
               (case runCLparser parseTargetAndArgs_ dict (drop 1 elems) of
                Right ((target,args),[]) -> 
                  return (FCE_ { 
                    leStamp = ts,
                    leFname = fn,
                    leCname = cn,
                    leCallerName  = crFn,
                    leCallerCName = crCn,
                    leTarget = target,
                    leArgs   = args })
                    
                _  -> fail "Parsing an FCE event. Error in parsing its target or arguments."
                           `withEvidence` [s]
               )
           
           _  -> fail "Parsing an FCE event. Error in parsing its caller name."
                      `withEvidence` [s]
      )                              
    }        

parseCaller = do 
  {
    dict <- getDict_ ;
    p@(Par par_)  <-  satisfy_ isParagraph `elseError` "Parsing caller name. Expecting a paragraph";
    case getSentences_ dict par_ of 
      Just [s] -> return (splitFNCN s)
      _   -> fail "Parsing caller name, but the number of sentences is wrong (should be 1)"
                  `withEvidence` [p]
  }
  
parseFCX_ :: String -> SemiCompressedLogEntry -> CompressedLogParser LowEvent_      
parseFCX_  tagTail s@(Section ts _ elems) = 
    let 
    (fn,cn) = splitFNCN tagTail
    in
    if not(length elems == 4)
    then fail "Parsing an FXE event. Error in its number of elements." `withEvidence` [s]
    else do {
      dict <- getDict_ ;
      (case runCLparser parseCaller dict [elems!!0] of
           Right ((crFn,crCn),[]) ->
               (case runCLparser (all_ parseValue) dict (drop 1 elems) of
                Right ([target,ret,exc],[]) -> 
                  return (FCX_ { 
                        leStamp = ts,
                        leFname = fn,
                        leCname = cn,
                        leCallerName  = crFn,
                        leCallerCName = crCn,
                        leTarget = target,
                        leReturn = ret,
                        leException = exc
                        })
                _  -> fail "Parsing an FCX event. Error in parsing its target or return value or thrown-exception value." 
                     `withEvidence` [s]
               )
           _   -> fail "Parsing an FCX event. Error in parsing its caller name."
                       `withEvidence` [s]
      )
    }        

    
parseBlock_ :: String -> SemiCompressedLogEntry -> CompressedLogParser LowEvent_       
parseBlock_  tagTail s@(Section ts _ elems) = 
   let 
   (bid,fn,cn) = splitBidFNCN tagTail
   in
   return (B_ { leStamp  = ts,
                leBId     = bid,
                leFname  = fn,
                leCname  = cn
               })   
                               
splitBlockTag_ tag = (bid, fn, drop 1 cn_)
   where
   tag_            = drop 2 tag 
   (bid,fnamePart) = span (/= ':') tag_  
   (fn,cn_)        = span (/= ':') (drop 1 fnamePart)
        

parseBEH_ :: String -> SemiCompressedLogEntry -> CompressedLogParser LowEvent_ 
parseBEH_  tagTail s@(Section ts _ elems) = 
   let 
   (bid,fn,cn) = splitBidFNCN tagTail
   in
   do {
      dict <- getDict_ ;
      if not (length elems == 1)
      then fail "Parsing a BEH event. Error in number of elements"
                `withEvidence` [s]
                
      else (case runCLparser (all_ parseValue) dict elems of
              Right ([exc],[]) -> 
                  return (BEH_ { leStamp  = ts,
                                 leBId     = bid,
                                 leFname  = fn,
                                 leCname  = cn,
                                 leException = exc
                                })
              _   ->  fail "Parsing a BEH event. Failing on its elements"
                           `withEvidence` [s]
           )                    
   }            

            
parseBLE_ :: String -> SemiCompressedLogEntry -> CompressedLogParser LowEvent_ 
parseBLE_  tagTail s@(Section ts _ elems) = 
   let 
   (bid,fn,cn) = splitBidFNCN tagTail
   in
   return (BLE_ { leStamp  = ts,
                  leBId     = bid,
                  leFname  = fn,
                  leCname  = cn
                })    
          

-- contains a HACK!
parseBLX_ :: String -> SemiCompressedLogEntry -> CompressedLogParser LowEvent_
parseBLX_  tagTail s@(Section ts _ elems) = 
   let 
   (bid,fn,cn) = splitBidFNCN tagTail
   in
   do {
      dict <- getDict_ ;
      if not(length elems <= 1)
      then fail "Parsing a BLX event. Error in its number of elements"
      
      else (case runCLparser (all_ parseValue) dict elems of
               Right ([],[]) -> return (BLX_ { 
                                      leStamp  = ts,
                                      leBId     = bid,
                                      leFname  = fn,
                                      leCname  = cn,
                                      leCnt    = Nothing 
                                    })
               Right ([SingleVal_ { vVal=v, vTy=ty }], []) ->        
                            return (BLX_ { 
                              leStamp  = ts,
                              leBId     = bid,
                              leFname  = fn,
                              leCname  = cn,
                              -- HACK: will fail if v is not parsable to Integer; fix this.
                              leCnt    = Just (read v)
                              })
               _  -> fail "Parsing a BLX event. Failing on its elements"
                          `withEvidence` [s]
           )                    
   }            


{- ==============================================================
   The XML formater part
============================================================== -}

instance ConvertibleToXML LowEvent_ where

   toXML (FE_ { 
                    leStamp  = ts_ ,
                    leFname  = fn ,
                    leCname  = cn ,
                    leTarget = targ ,
                    leArgs   = args
                 })
       =
       mkElem "FE" (ts ++ f)  [toXML targ, argsXML]
       where
       ts = case ts_ of
              Nothing          -> []
              Just (offset,t)  -> [attrib1 "t" (show offset ++ ":" ++ show t) ]
       f  = [attrib1 "f" (fn ++ ":" ++ cn)]
       argsXML = mkElem "args" [] (Data.List.map toXML args)

   toXML (FX_ { 
                    leStamp  = ts_ ,
                    leFname  = fn ,
                    leCname  = cn ,
                    leTarget = targ ,
                    leReturn = ret
                 })
       =
       mkElem "FX" (ts ++ f)  (Data.List.map toXML [targ,ret])
       where
       ts = case ts_ of
              Nothing          -> []
              Just (offset,t)  -> [attrib1 "t" (show offset ++ ":" ++ show t) ]
       f  = [attrib1 "f" (fn ++ ":" ++ cn)]
       
   toXML (FCE_ { 
                    leStamp  = ts_ ,
                    leFname  = fn ,
                    leCname  = cn ,
                    leCallerName   = ceFn ,
                    leCallerCName  = ceCn ,
                    leTarget = targ ,
                    leArgs   = args
                 })
       =
       mkElem "FCE" (ts ++ f ++ ce)  [toXML targ, argsXML]
       where
       ts = case ts_ of
              Nothing          -> []
              Just (offset,t)  -> [attrib1 "t" (show offset ++ ":" ++ show t) ]
       f   = [attrib1 "f" (fn ++ ":" ++ cn)]
       ce  = [attrib1 "ce" (ceFn ++ ":" ++ ceCn)]
       argsXML = mkElem "args" [] (Data.List.map toXML args)

   toXML (FCX_ { 
                    leStamp  = ts_ ,
                    leFname  = fn ,
                    leCname  = cn ,
                    leCallerName   = ceFn ,
                    leCallerCName  = ceCn ,
                    leTarget = targ ,
                    leReturn = ret,
                    leException = exc
                 })
       =
       mkElem "FCX" (ts ++ f ++ ce) (Data.List.map toXML [targ,ret,exc])
       where
       ts = case ts_ of
              Nothing          -> []
              Just (offset,t)  -> [attrib1 "t" (show offset ++ ":" ++ show t) ]
       f  = [attrib1 "f" (fn ++ ":" ++ cn)]
       ce = [attrib1 "ce" (ceFn ++ ":" ++ ceCn)]
   
   toXML (B_ { 
                    leStamp  = ts_ ,
                    leBId     = bid ,
                    leFname  = fn ,
                    leCname  = cn
                 })
       =
       mkElem "B" (ts ++ f ++ i) []
       where
       ts = case ts_ of
              Nothing          -> []
              Just (offset,t)  -> [attrib1 "t" (show offset ++ ":" ++ show t) ]
       f  = [attrib1 "f" (fn ++ ":" ++ cn)]
       i  = [attrib1 "i" bid]
       
   toXML (BEH_ { 
                    leStamp  = ts_ ,
                    leBId     = bid ,
                    leFname  = fn ,
                    leCname  = cn ,
                    leException = exc
                 })
       =
       mkElem "BEH" (ts ++ f ++ i) [toXML exc]
       where
       ts = case ts_ of
              Nothing          -> []
              Just (offset,t)  -> [attrib1 "t" (show offset ++ ":" ++ show t) ]
       f  = [attrib1 "f" (fn ++ ":" ++ cn)]
       i  = [attrib1 "i" bid]
       
   toXML (BLE_ { 
                    leStamp  = ts_ ,
                    leBId     = bid ,
                    leFname  = fn ,
                    leCname  = cn
                 })
       =
       mkElem "BLE" (ts ++ f ++ i) []
       where
       ts = case ts_ of
              Nothing          -> []
              Just (offset,t)  -> [attrib1 "t" (show offset ++ ":" ++ show t) ]
       f  = [attrib1 "f" (fn ++ ":" ++ cn)]
       i  = [attrib1 "i" bid]

   toXML (BLX_ { 
                    leStamp  = ts_ ,
                    leBId     = bid ,
                    leFname  = fn ,
                    leCname  = cn ,
                    leCnt    = cnt_ 
                 })
       =
       mkElem "BLX" (ts ++ f ++ i ++ cnt) []
       where
       ts = case ts_ of
              Nothing          -> []
              Just (offset,t)  -> [attrib1 "t" (show offset ++ ":" ++ show t) ]
       f  = [attrib1 "f" (fn ++ ":" ++ cn)]
       i  = [attrib1 "i" bid]  
       cnt =  case cnt_ of
              Nothing          -> []
              Just i  -> [attrib1 "cnt" (show i)]    

              
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
    
exBlockLog1  = testLowLevelParser_  (all_ parseLowEvent)  exBlockLog1_ 
exBlockLog1_ = internalRepRawLog__ exBlockLog1__
   
exBlockLog1__  =  concat . intersperse "\n" $
        ["%<S -120:1318674228273 \"B:2177:recGcd:GCDplain\" %>",
         "%<S -120:1318674228273 \"B:2178:recGcd:GCDplain\" %>",
         "%<S \"B:2182:recGcd:GCDplain\" %>"
        ] 
  
 
exBEHLog1  = testLowLevelParser_  (all_ parseLowEvent)  exBEHLog1_ 
exBEHLog1_ = internalRepRawLog__ exBEHLog1__
exBEHLog1__  = concat . intersperse "\n" $
        ["%<S -120:1318674228273 \"BEH:2177:recGcd:GCDplain\" ",
         "%<P %<{ \"an exception\":String }%> %>",
         "%>"
        ] 

       
exBLELog1  =  testLowLevelParser_  (all_ parseLowEvent)  exBLELog1_  
exBLELog1_ = internalRepRawLog__ exBLELog1__
exBLELog1__  = concat . intersperse "\n" $
        ["%<S -120:1318674228273 \"BLE:2177:recGcd:GCDplain\" %>",
         "%<S \"BLE:2182:recGcd:GCDplain\" %>"
        ]
        
exBLXLog1  =  testLowLevelParser_  (all_ parseLowEvent) exBLXLog1_  
exBLXLog1_ =  internalRepRawLog__ exBLXLog1__
exBLXLog1__ = concat . intersperse "\n" $
        ["%<S -120:1318674228273 \"BLX:2177:recGcd:GCDplain\" ",
         "%<P %<{ 10:int }%> %>",
         "%>"
        ] 
        
exFELog1  =  testLowLevelParser_  (all_ parseLowEvent) exFELog1_  
exFELog1_ =  internalRepRawLog__ exFELog1__        
exFELog1__ = concat . intersperse "\n" $
        ["%<S -120:1318674228273 \"FE:recGcd:GCDplain\" ",
         "%<P %<{ \"target\":string }%> %>",
         "%<S \"args\"  %<P %<{ \"arg0\":string }%> %> %>",
         "%>"
        ]    

exFXLog1  =  testLowLevelParser_  (all_ parseLowEvent) exFXLog1_  
exFXLog1_ =  internalRepRawLog__ exFXLog1__          
exFXLog1__ = concat . intersperse "\n" $
        ["%<S -120:1318674228273 \"FX:recGcd:GCDplain\" ",
         "%<P %<{ \"target\":string }%> %>",
         "%<P %<{ \"a return value...\":string }%> %>",
         "%>"
        ]    

exFCELog1  =  testLowLevelParser_  (all_ parseLowEvent) exFCELog1_  
exFCELog1_ =  internalRepRawLog__ exFCELog1__     
exFCELog1__ = concat . intersperse "\n" $
        ["%<S -120:1318674228273 \"FCE:recGcd:GCDplain\" ",
         "%<P %<{ someCallerMethod:SomeClass }%> %>",
         "%<P %<{ \"target\":string }%> %>",
         "%<S \"args\"  %<P %<{ \"arg0\":string }%> %> %>",
         "%>"
        ]
        
exFCXLog1  =  testLowLevelParser_  (all_ parseLowEvent) exFCXLog1_  
exFCXLog1_ =  internalRepRawLog__ exFCXLog1__     
exFCXLog1__ = concat . intersperse "\n" $
        ["%<S -120:1318674228273 \"FCX:recGcd:GCDplain\" ",
         "%<P %<{ someCallerMethod:SomeClass }%> %>",
         "%<P %<{ \"target\":string }%> %>",
         "%<P %<{ \"some ret val\":string }%> %>",
         "%<P %<{ null:Null }%> %>",
         "%>"
        ]
        
exLowLevelLog1  =  testLowLevelParser_  (all_ parseLowEvent)  (internalRepRawLog__ log)
    where
    log = exBlockLog1__  ++ exBEHLog1__ ++ exBLELog1__ ++ exBLXLog1__
          ++ exFELog1__ ++ exFXLog1__  ++  exFCELog1__  ++  exFCXLog1__
    