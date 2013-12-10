{- 

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 

   This module provides some utilities related to log format 
   conversions:
   
   - some parser combinators to parse compressed raw log
   - XML related utitlity functions
     
-}

module Eu.Fittest.Logging.XML.Base(
    -- CanBeObtainedFromCompressedLog(..),
    SemiCompressedLogEntry,
    CompressedLogParser(..),
    topRunCLparser,
    (<<|),
    satisfy_,
    getDict_,
    eof_,
    elseError,
    withEvidence,
    all_,
    ConvertibleToXML(..),
    attrib1,
    attribs,
    mkElem
   )

where

import Text.XML.Light
import Eu.Fittest.Logging.Compression.RawlogParser
import Eu.Fittest.Logging.Compression.Compress

{- ===========================================================================
    Defining parser combinators to parse compressed log
============================================================================ -}

{-
class CanBeObtainedFromCompressedLog a where
    clParse :: Dictionary -> CompressedLogEntry -> a
-}

-- | Compressed raw log, but with timestamps decompressed.
type SemiCompressedLogEntry = RawLogEntry UTCTimeStamp1970 SectionTag_ Paragraph_

    
-- | A (monadic) type representing parsers over semi-compressed raw logs.  
data CompressedLogParser v 
     = 
     CLParser { runCLparser ::  Dictionary 
                                -> [SemiCompressedLogEntry] 
                                -> Either (String,[SemiCompressedLogEntry]) -- error
                                          (v,[SemiCompressedLogEntry])      -- real result
              }

-- | To run the a semi-compressed log parser from top level.              
topRunCLparser parser dict log = case runCLparser parser dict log of
   Right (v,[])  -> v
   
   Right (_,s)   -> error ("Fail to consume this sufix (showing at most the first 3 entries):\n\n" 
                            ++ show (take 3 s))
                            
   Left  (msg,s) -> error (msg ++ ", on this fragment (showing at most the first 3 entries):\n\n" 
                               ++ show (take 3 s))   

instance Monad CompressedLogParser where
    CLParser f >>= g  = CLParser { runCLparser = h }
       where
       h dict log = case f dict log of
                       Right (v,log2) -> runCLparser (g v) dict log2
                       Left e  ->  Left e
                       
    return v   = CLParser { runCLparser = (\dict log -> Right (v,log)) }
    
    -- | This parser always fail, and leaves msg as the error message and current log itself as evidence.
    fail msg   = CLParser { runCLparser = (\dict log -> Left (msg,log)) }
    
-- | Left biased choice of parsers g1 and g2.   
g1 <<| g2 = CLParser { runCLparser = h }
    where
    h dict log = case runCLparser g1 dict log of
                    Left _  -> runCLparser g2 dict log
                    v       -> v
                
-- | This parser consumes the current log entry if it satisfies p, else it fails.              
satisfy_ p = CLParser { runCLparser = h }
    where                   
    h _ [] = Left ("Applying satisfy_ on empty sufix",[])
    h _ (entry:rest) = if p entry 
                          then Right (entry,rest) 
                          else Left  ("satisfy_ check fails",[entry])
  
-- | This parser does not consume any entry, but returns the log's dictionary.
getDict_ =  CLParser { runCLparser = (\dict log -> Right (dict,log)) }

-- | This parser returns val if the log has no entry left. Else it fails.
eof_ val = CLParser { runCLparser = h }
    where
    h _ []   = Right (val,[])
    h _ log  = Left ("Expecting end of input",log)
      
-- | Parse with p. If it fails, msg is the error message.
p `elseError` msg = p <<| fail msg 
    
-- | Parse with p. If it fails, z is set as the evidence.
p `withEvidence` z = CLParser { runCLparser = h }
    where
    h dict log = case runCLparser p dict log of
                    Left (msg,_)  -> Left (msg,z)
                    v             -> v
                    
-- | Greedilly parse g* until the end of input.        
all_ g = eof_ []
         <<|
         do { x <- g ; s <- all_ g ; return (x:s) }         
                    
infixr 3 <<|   


{- ===========================================================================
    Some useful XML functions
============================================================================ -}
    
-- | Representing types which are convertible to XML.    
class ConvertibleToXML t where
   -- | Convert to an XML-element.
   toXML :: t -> Element          
    
-- | Construct a single XML-element's attribute of name=val.    
attrib1 name val = Attr { attrKey = unqual name,
                          attrVal  = val }

-- | Construct a list of attributes.                        
attribs = map (uncurry attrib1)

-- | Construct a single XML-element with the specified tag, attributes, and sub-elements.
mkElem tag attribs subelements  =  Element { 
             elName = unqual tag,
             elAttribs = attribs,
             elContent = map Elem subelements,
             elLine = Nothing
           }  
           
                         
  