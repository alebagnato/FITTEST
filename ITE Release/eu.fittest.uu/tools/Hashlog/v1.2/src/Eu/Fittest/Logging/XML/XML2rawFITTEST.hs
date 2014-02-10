{- 

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 

   This module provides a function to convert a FITTEST XML log file, 
   to raw FITTEST log.
   
-}

module Eu.Fittest.Logging.XML.XML2rawFITTEST(
         xml2raw
     )
    
where
    
import Eu.Fittest.Logging.XML.XMLparser
import Eu.Fittest.Logging.XML.ToFITTESTRawLog
import qualified Data.ByteString.Lazy as B
import System.FilePath

xml2raw :: FilePath -> FilePath -> IO()
xml2raw filename1 filename2 = do
  events <- parseXMLlog filename1
  let rawlog = events2raw_bytestring events
      
  let (baseName,ext) = splitExtension filename1 
  let rawLogFileName = if null filename1
                       then filename2
                       else baseName ++ ".log"
  B.writeFile rawLogFileName rawlog
  
-- example
exLog = xml2raw "./SampleXMLLog1.xml" 

  
  
  
  