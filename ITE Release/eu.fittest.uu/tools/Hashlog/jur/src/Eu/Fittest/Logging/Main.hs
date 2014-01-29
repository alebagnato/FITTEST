{-

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- |

   Provides a top level tool to compress log, and convert to XML.

-}
module Main

where

import Data.List
import System.Environment
import Eu.Fittest.Logging.Compression.RawlogParser
import Eu.Fittest.Logging.Compression.Serializer
import Eu.Fittest.Logging.XML.EventLog
import Eu.Fittest.Logging.Daikon.DaikonConverter
import Eu.Fittest.Logging.Daikon.LowLevelOracle.LLOBase

main :: IO()
main = do { args <- getArgs ; main_ args }

mainf args = main_ (words args)

main_ args = do {
   case args of
      ("--rstats" : fn : _)  ->  printStatistics fn
      ("--compress" : fn : _)  ->  compressThenSaveRawLog fn
      ("--toxml" : fn : _)     ->  saveAsXML fn
      ("--htoxml" : fn :_)     ->  filterAppEventsThenSaveAsXML fn
      ("--daikon" : fn :_)     ->  saveAsDaikonLog fn
      ("--llo" : str : fn : _) ->  saveAsLLODaikonLog (read str) fn
      ("--help" : _)           ->  putStrLn (concat . intersperse "\n" $ infos)
      _  -> putStrLn "** Error:options not recognized."
   }
   where
   infos = ["--rstats <name of file containing raw log>  (print raw log statistics)",
            "--compress <name of file containing raw log>  (save raw log in compressed form)",
            "--toxml  <name of file containing compressed log> (convert compressed log to XML)",
            "--htoxml <name of file containing compressed log> (convert only high level events in a compressed log to XML)",
            "--daikon <name of file containing compressed log> (convert the log to daikon-format)",
            "--llo <int> <name of file containing compressed log> (produce a daikon log containing control flows info, for oracle inference)",
            "--help ... obvious :)" ]

