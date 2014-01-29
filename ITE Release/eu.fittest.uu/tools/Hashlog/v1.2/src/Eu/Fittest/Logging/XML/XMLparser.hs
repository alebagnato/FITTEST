{- 

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 

   This module provides a parser to parse a FITTEST XML log file, and 
   to convert it to an internal representation of events list.
   
-}

module Eu.Fittest.Logging.XML.XMLparser(
         parseXMLlog
     )

where

import Text.XML.Light
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Data.Text.IO
import qualified Data.Text as T

import Eu.Fittest.Logging.Compression.RawlogParser
import Eu.Fittest.Logging.XML.Event
import Eu.Fittest.Logging.XML.AppEvent
import Eu.Fittest.Logging.XML.LowEvent
import Eu.Fittest.Logging.XML.Value

  
-- a function to parse a file containing FITTEST XML log, to
-- a sequence of events (in an internal representation).
--  
parseXMLlog :: String -> IO [Event_]
parseXMLlog fileName =
  do 
  log <- Data.Text.IO.readFile fileName
  let contents = parseXML log 
  return (pLogEntries contents)

      
-- A function to parse the content of an XML file.
-- Note the the content is a list of contents.
pLogEntries :: [Content] -> [Event_]
pLogEntries contents = case sequence entries of
                         Just events -> events
                         Nothing     -> error "pLogEntries: fail to parse the XML log."
  where
  entries = [pEntry e | e <- getLogEntries contents]
  
-- just a helper to get the list of XML-elements from
--  'contents'
getLogEntries :: [Content] -> [Element]
getLogEntries xmllog = concat [ elChildren b | b <- bodies ] 
  where
  bodies = [ b | b <- onlyElems xmllog, isBody b]
  isBody (Element (QName "body" Nothing Nothing) _ _ _) = True
  isBody _  =  False

{- =====================================================================
   This part defines the parser. The component-parsers usually take
   a single XML-elemement, and try to parse it to the corresponding
   internal representation of events (or its elements). The result is
   usually of type Maybe a, where Nothing represents failure to parse.
===================================================================== -}

-- top level, parser of an Event
pEntry :: Element -> Maybe Event_ 
pEntry = (AE <$> pAppEvent) <|> (LE <$> pLowEvent)

-- defining some parser combinators
infixr 5 <|>
infixl 8 <$>

(f <$> p) x = do { r <- p x ; return (f r) }           
(p <|> q) x = case p x of
                Just x -> Just x
                _      -> q x
                
               
-- helper, construct a simple QName                     
mkSimpleName name = QName name Nothing Nothing

-- to parse timestamp from a list of attributes
pTimestamp :: [Attr] -> Maybe UTCTimeStamp1970
pTimestamp attribs = 
   do
   t <- lookupAttr (mkSimpleName "t") attribs 
   -- break timestamp to a pair (offset,time) :
   let (offset,(_:t'))  =  break (== ':') t
   return (read offset, read t')
    
-- to parse an application event
pAppEvent :: Element -> Maybe AppEvent_  
pAppEvent e@(Element (QName "E" Nothing Nothing) attribs _ _) 
   = 
   do 
   let elems = elChildren e
   if isTwo elems then return () 
                  else fail ("AppEvent requires two objects, , it gets: " ++ show e)
   let [event,state] = elems
   e  <- pObj event 
   st <- pObj state 
   return (AppEvent_ (pTimestamp attribs) e st) 
   
pAppEvent e@(Element (QName "S" Nothing Nothing) attribs _ _) 
   = 
   do 
   let elems = elChildren e
   if isSing elems then return () 
                   else fail ("AppState requires one object, it gets: " ++ show e)
   state <- pValue (head elems)
   return (AppState_ (pTimestamp attribs) state) 
   
-- else it does not match:         
pAppEvent _ = Nothing


-- to parse (callee) function:class names from a list of attributes
pFuncAndClassNames :: [Attr] -> Maybe (String,String)
pFuncAndClassNames attribs = 
   do
   name <- lookupAttr (mkSimpleName "f") attribs 
   let (fn,(_:cn))  =  break (== ':') name
   return (fn,cn)
   
-- to parse (caller) function:class names from a list of attributes
pCallerFuncAndClassNames :: [Attr] -> Maybe (String,String)
pCallerFuncAndClassNames attribs = 
   do
   name <- lookupAttr (mkSimpleName "ce") attribs 
   let (fn,(_:cn))  =  break (== ':') name
   return (fn,cn)   
   
-- to parse a low level event:
pLowEvent :: Element -> Maybe LowEvent_
pLowEvent e@(Element (QName name Nothing Nothing) attribs _ _) 
   =
   do
   (funcName,className) <- pFuncAndClassNames attribs
   let tstamp = pTimestamp attribs
   let elems  = elChildren e
   case name of
     "FE" -> do
             if isTwo elems then return () 
                            else fail ("FE requires two objects, but it is: " ++ show e)  
             let [tobj,args] = elems
             tobj_ <- pValue tobj   
             args_ <- pArgs args
             return (FE_ tstamp funcName className tobj_ args_)  

     "FX" -> do
             if isTwo elems then return () 
                            else fail ("FX requires two objects, but it is: " ++ show e)  
             [tobj,ret] <- sequence [ pValue x | x <- elems ]
             return (FX_ tstamp funcName className tobj ret) 
             
     "FCE" -> do
              (fname2,cname2) <- pCallerFuncAndClassNames attribs
              if isTwo elems then return () 
                             else fail ("FCE requires two objects,  but it is: " ++ show e)  
              let [tobj,args] = elems
              tobj_ <- pValue tobj   
              args_ <- pArgs args
              return (FCE_ tstamp funcName className fname2 cname2 tobj_ args_) 
              
     "FCX" -> do
              (fname2,cname2) <- pCallerFuncAndClassNames attribs
              if is3 elems then return () 
                           else fail ("FCX requires 3 objects, but it is: " ++ show e)  
              [tobj,ret,exc] <- sequence [pValue x | x <- elems]
              return (FCX_ tstamp funcName className fname2 cname2 tobj ret exc) 

     "B" -> do
            bid <- lookupAttr (mkSimpleName "i" ) attribs
            return (B_ tstamp bid funcName className)    

     "BEH" -> do
              bid <- lookupAttr (mkSimpleName "i" ) attribs
              if isSing elems then return () 
                              else fail ("BEH requires one object, but it is: " ++ show e)
              exc <- pValue . head $ elems                              
              return (BEH_ tstamp bid funcName className exc)  
              
     "BLE" -> do
              bid <- lookupAttr (mkSimpleName "i" ) attribs
              return (BLE_ tstamp bid funcName className) 
            
     "BLX" -> do
              bid <- lookupAttr (mkSimpleName "i" ) attribs
              let cnt = pLoopCounter attribs                              
              return (BLX_ tstamp bid funcName className cnt)  
     -- else it does not match: 
     _  ->  Nothing 

-- else it does not match:   
pLowEvent _  =  Nothing         
    
pLoopCounter :: [Attr] -> Maybe Integer
pLoopCounter attribs = 
  do 
  c <- lookupAttr (mkSimpleName "cnt" ) attribs
  return (read c)
            
pArgs :: Element -> Maybe [Value_]
pArgs  e@(Element (QName "args" Nothing Nothing) attribs _ _) 
   =
   do
   let elems = elChildren e
   sequence [ pValue x | x <- elems ]
-- else it does not match:   
pArgs _ = Nothing
          
-- to parse a value
pValue :: Element -> Maybe Value_
pValue e@(Element (QName "V" Nothing Nothing) attribs _ _) =
    do
    v  <- lookupAttr (mkSimpleName "v" ) attribs
    ty <- lookupAttr (mkSimpleName "ty") attribs
    return (SingleVal_ v ty)
-- else:
pValue e = pObj e  

-- to parse an Object
pObj :: Element -> Maybe Value_
pObj e@(Element (QName "O" Nothing Nothing) attribs _ _) = do {
      className <- lookupAttr (mkSimpleName "ty") attribs ;
      fields <- sequence [ pField f | f <- elChildren e ] ;
      return (Obj_ className fields) ;      
  }
-- else it does not match:   
pObj _  =  Nothing  

-- a bunch of helpers

getElementSimpleName (Element (QName name Nothing Nothing) _ _ _) = Just name
getElementSimpleName _  = Nothing

isSing [_] = True
isSing _   = False

isTwo [_,_] = True
isTwo _     = False

is3 [_,_,_] = True
is3  _      = False


-- to parse a field
pField e@(Element (QName "fd" Nothing Nothing) attribs _ _) 
   =
   do
   fieldName <- lookupAttr (mkSimpleName "n") attribs
   let elems = elChildren e
   if isSing elems then return () else fail ("Field requires one object: " ++ show e)
   let [o] = elems
   o_name <- getElementSimpleName o
   let o_attribs = elAttribs o
   case o_name of
     "V"  ->  do             
              v  <- lookupAttr (mkSimpleName "v" ) o_attribs
              ty <- lookupAttr (mkSimpleName "ty") o_attribs
              return (FieldValTy_ fieldName v ty)
     "X"  ->  do
              target <- lookupAttr (mkSimpleName "to" ) o_attribs
              return (FieldXref_ fieldName (read target))
     "O"  ->  do
              subobj <- pObj o
              return (FieldObj_ fieldName subobj)
     _    ->  fail ("Unknown field-type: " ++ show e)
-- else it does not match:   
pField _  = Nothing  
  
--
-- test examples:
--    
    
exLog1 = 
    -- just to print back rawly parsed XML
    do 
      log <- Data.Text.IO.readFile "./SampleXMLLog1.xml" 
      let contents = parseXML log 
      let entries  = getLogEntries contents
      let e = entries !! 0
      Data.Text.IO.putStr (T.pack . show $ e) 

exLog2 = do 
         -- printing events parsed from XML
         events <- parseXMLlog "./SampleXMLLog1.xml" 
         sequence_ [Prelude.putStrLn (show e) | e<-events]