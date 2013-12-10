{- 

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 

   DEPRACATED!!!

   This module provides a type representing a sequence of application-events which
   are to be read from a log, and:

   * a parser to read from compressed log.
   * XML formater.   
     
-}

module Eu.Fittest.Logging.XML.AppEventLog

where

import System.FilePath
import Text.XML.Light
import Eu.Fittest.Logging.XML.Base
import Eu.Fittest.Logging.XML.Value
import Eu.Fittest.Logging.XML.AppEvent
import Eu.Fittest.Logging.Compression.RawlogParser
import Eu.Fittest.Logging.Compression.Compress
import Eu.Fittest.Logging.Compression.Serializer

-- | Parsing a sequence of app-events from a compressed log. Note that 
-- this function assumes the following:
--  
--     * timestamps are uncompressed
--     * the log only contains app-events
--
parseAppEventLog :: CompressedLogParser [AppEvent_]
parseAppEventLog = all_ parseAppEvent

-- | To convert a sequence of events to its XML representation.
eventLog2XML :: [AppEvent_] -> Element
eventLog2XML events = mkElem "body" [] (map toXML events)

-- | Will pretty print a sequence of events in XML format.
ppXMLEventLog :: [AppEvent_] -> String
ppXMLEventLog = ppTopElement . eventLog2XML

-- | Parse a sequence of app-events from a compressed log, and pretty
--   print it in XML format. Note that this function assumes that
--   the log only contains app-events. Timestamps in the log should be
--   in compressed format.
--
parseAndXMLPrintEventLog :: Dictionary -> [CompressedLogEntry] -> String
parseAndXMLPrintEventLog dict log = ppXMLEventLog events
    where
    events = topRunCLparser parseAppEventLog dict log2
    log2   = decompressTimeStamp log
    

-- | Read and save a compressed log in an XML format. The function
--   will ignore top-entries which are not app-events. Timestamps
--   in the input file should be left compressed.
--
saveAsXML :: FilePath -> IO()
saveAsXML file = do {
      (dict,log) <- loadCompressedLog file ;
      log2 <- return (topfilterLog timePred tagPred (dict,log)) ;
      output     <- return (parseAndXMLPrintEventLog dict log2) ; 
      writeFile xmlf output
   }
   where
   timePred t   = True
   tagPred  tag = tag == "E"
   basef = dropExtension file
   xmlf  = addExtension basef "xml"
 
 
