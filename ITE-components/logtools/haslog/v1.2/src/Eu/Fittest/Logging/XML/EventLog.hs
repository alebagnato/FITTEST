{- 

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 

   This module provides a type representing a sequence of events which
   are to be read from a log, and:

   * a parser to read from semi-compressed raw log.
   * XML formater.   
     
-}

module Eu.Fittest.Logging.XML.EventLog

where

import System.FilePath
import Text.XML.Light
import Eu.Fittest.Logging.XML.Base
import Eu.Fittest.Logging.XML.Event
import Eu.Fittest.Logging.Compression.RawlogParser
import Eu.Fittest.Logging.Compression.Compress
import Eu.Fittest.Logging.Compression.Serializer

-- | Parsing a sequence of app-events from semi-compressed raw log. Semi-compressed
-- means that the timestamps have been decompressed.
--
parseEventLog :: CompressedLogParser [Event_]
parseEventLog = all_ parseEvent

-- | To convert a sequence of events to its XML representation.
eventLog2XML :: [Event_] -> Element
eventLog2XML events = mkElem "body" [] (map toXML events)

-- | Will pretty print a sequence of events in XML format.
ppXMLEventLog :: [Event_] -> String
ppXMLEventLog = ppTopElement . eventLog2XML

-- | Parse a sequence of events from a compressed raw log, and pretty
--   print it in XML format. Timestamps in the log should be
--   in compressed format.
--
parseAndXMLPrintEventLog :: Dictionary -> [CompressedLogEntry] -> String
parseAndXMLPrintEventLog dict log = ppXMLEventLog events
    where
    events = topRunCLparser parseEventLog dict log2
    log2   = decompressTimeStamp log
    

-- | Read and save a compressed log in an XML format. Timestamps
--   in the input file should be left compressed.
--
saveAsXML :: FilePath -> IO()
saveAsXML file = do {
      (dict,log) <- loadCompressedLog file ;
      output     <- return (parseAndXMLPrintEventLog dict log) ; 
      writeFile xmlf output
   }
   where
   basef = dropExtension file
   xmlf  = addExtension basef "xml"
   
-- | Read a compressed log, and save only the application
-- events in an XML format. In particular, entries representing
-- low level events are not saved. Timestamps in the input file 
-- should be left compressed.
--   
filterAppEventsThenSaveAsXML :: FilePath -> IO()
filterAppEventsThenSaveAsXML file = do {
      (dict,log) <- loadCompressedLog file ;
      log2 <- return (topfilterLog timePred tagPred (dict,log)) ;
      output     <- return (parseAndXMLPrintEventLog dict log2) ; 
      writeFile xmlf output
   }
   where
   timePred t   = True
   tagPred  tag = elem tag ["E","S"]
   basef = dropExtension file
   xmlf  = addExtension basef "xml"
 
 
