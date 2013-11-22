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

import System.Console.GetOpt
import Eu.Fittest.Utils.GetOpt
import Data.List
import System.Environment
import Eu.Fittest.Logging.Compression.RawlogParser
import Eu.Fittest.Logging.Compression.Serializer
import Eu.Fittest.Logging.XML.EventLog
import Eu.Fittest.Logging.XML.XML2rawFITTEST
import Eu.Fittest.Logging.Daikon.DaikonConverter
import Eu.Fittest.Logging.Daikon.LowLevelOracle.LLOBase


main :: IO()
main = do { args <- getArgs ; 
            putStrLn "**" ;
            putStrLn ("** Receiving these args from system: " ++ show args) ;
            putStrLn "**" ;
            main_ args }

-- | This interpret the options from the string.
mainf :: String -> IO()
mainf args = main_ (words args)

main_ :: [String] ->IO()
main_ args = case getOpt Permute options args of
   (o,n,[]) -> if CmdHelp `elem` o || null n then help
               else interpret o (head n) (getParamOutFile o)

   (_,_,errs) -> ioError (userError (concat errs ++ help_))
   
   where
   -- the actual interpretation is here
   interpret o fn outputFile
     | CmdRawLogStat `elem` o =  printStatistics fn           -- printing statistics
     | CmdCompress `elem` o   =  compressThenSaveRawLog fn    -- compressing
     | CmdToRaw `elem` o      =  xml2raw fn                   -- converting fn back to raw log
     | CmdToXML `elem` o      = if FilterInclOnlyAppEvents `elem` o       -- XML format
                                   then filterAppEventsThenSaveAsXML fn
                                   else saveAsXML fn
                                   
     | CmdToDaikon `elem` o   = -- daikon format
           let 
           printStatFlag = FlagDaikonStat `elem` o
           daikonOptions = DConvOptions {
             dcOptGenerateAuxVars = FlagDaikonGenAuxVars `elem` o,
             dcOptRegexVarsChoice = getParamRegexVarsChoice_ o ,
             dcOptRegexAppEventsChoice = getParamRegexAppEventsChoice_ o         
           } 
           in 
           case getParamLLO_ o of     
                Nothing -> saveAsDaikonLog printStatFlag daikonOptions fn outputFile
                Just functionNameRegex ->
                    let
                    lloOpts = LLOOptions {
                        lloOptRegexFunctionsChoice = functionNameRegex,
                        lloOptMode = getParamLLOlevel_ o ,
                        lloChopAt  = getParamLLOChopAt_ o
                      } 
                    in
                    saveAsLLODaikonLog printStatFlag daikonOptions lloOpts fn outputFile
      
     | otherwise = help

-- ============================================================================
-- Underlying type representing various options/flags/params + utilities
-- ============================================================================     
   
-- | Various options available for haslog
data Flag 
     = CmdHelp         -- printing help
     | CmdCompress     -- compress raw log
     | CmdToXML        -- convert to xml log
     | CmdToRaw        -- convert FITTEST-xml log back to FITTEST-raw log
     | FilterInclOnlyAppEvents  -- only incl. app events in xml
     | CmdToDaikon     -- convert to Daikon
     | CmdRawLogStat   -- printing raw log statistics
     | ParamOutFile String  -- name of output file; empty if it is unspecified
     | FlagDaikonStat  -- printing daikon log statistics
     | FlagDaikonGenAuxVars  -- will cause auxilliary variables to be generated in Daikon conversion
     | ParamRegexVarsChoice  String -- regular expression for selecting variables to include
     | ParamRegexAppEventsChoice String -- regular expression for selecting app-events to include
     | ParamLLO  String -- daikon, for low level oracles inference
     | ParamLLOlevel Int  -- daikon, specifying LLO level
     | ParamLLOChopAt Int -- daikon, llo, specifying where to chop off input
     deriving (Eq,Show)
     
isParamOutFile (ParamOutFile _) = True
isParamOutFile _ = False

getParamOutFile opts = case find isParamOutFile opts of
   Just (ParamOutFile f) -> f
   _  ->  ""
    
isParamLLO_ (ParamLLO _) = True
isParamLLO_  _  =  False

getParamLLO_ opts = case find isParamLLO_ opts of     
   Just (ParamLLO functionNameRegex) -> Just functionNameRegex
   _  ->  Nothing

isParamLLOlevel_ (ParamLLOlevel _) = True
isParamLLOlevel_  _  =  False   

getParamLLOlevel_ opts = case find isParamLLOlevel_ opts of
   Just (ParamLLOlevel i) -> i
   _   ->  1

isParamLLOChopAt_ (ParamLLOChopAt _) = True
isParamLLOChopAt_  _  =  False   

getParamLLOChopAt_ opts = case find isParamLLOChopAt_ opts of
   Just (ParamLLOChopAt i) -> Just i
   _   ->  Nothing  
   
isParamRegexVarsChoice_ (ParamRegexVarsChoice _) = True
isParamRegexVarsChoice_  _  = False
   
getParamRegexVarsChoice_ opts = case find isParamRegexVarsChoice_ opts of
    Just (ParamRegexVarsChoice regex) -> regex
    _  ->  []
                                      
isParamRegexAppEventsChoice_ (ParamRegexAppEventsChoice _) = True
isParamRegexAppEventsChoice_  _  = False   
 
getParamRegexAppEventsChoice_ opts = case find isParamRegexAppEventsChoice_ opts of
    Just (ParamRegexAppEventsChoice regex) -> regex
    _  ->  []       
   
-- ============================================================================
-- The description of various options
-- ============================================================================
   
options :: [OptDescr Flag]
options = getOptionsFromOptionTrees optionTrees

-- This is just the description of every option, structured hierarchically
optionTrees :: [OptionTree Flag]
optionTrees = [
     opt1 ""  ["help"] (NoArg CmdHelp) "Obvious.",
     opt1 "c" ["compress"] (NoArg CmdCompress) "Produce a compressed version of the given rawlog file (.log).",
     opt1 ""  ["rstat"] (NoArg CmdRawLogStat) "Print the statistics of the given rawlog file (.log).",
     opt1 ""  ["toraw"] (NoArg CmdToRaw) "Convert FITTEST-xml log (.xml) to FITTET-raw log (.log)",
     xmlOptions,
     daikonOptions                                
     ]
   where
   xmlOptions = optg "x" ["xml"] (NoArg CmdToXML)
                          "Produce an XML version of the given compressed log file (.lox)."                              
                    [opt1 "" ["appEventOnly"] (NoArg FilterInclOnlyAppEvents) "Include app.events only." ] 
                    
   daikonOptions = optg "d" ["daikon"] (NoArg CmdToDaikon) 
                             "Produce a Daikon-format version of the given compressed log file (.lox)."
                    [opt1 "" ["dstat"] (NoArg FlagDaikonStat)
                             "Print statistics (WARNING: may cause memory peak!).",
                     opt1 "o" ["output"] (ReqArg ParamOutFile "file")
                             "Output file",
                     opt1 "" ["genAuxvars"] (NoArg FlagDaikonGenAuxVars)
                             "Will cause auxiliary variables to be generated in the Daikon log.",
                     opt1 "" ["varsSelect"] (ReqArg ParamRegexVarsChoice "regex")
                             "To specify which state-variables are included in the produced Daikon log. Default is all.",
                     opt1 "" ["appEvsSelect"] (ReqArg ParamRegexAppEventsChoice "regex")
                             "To specify which application events are included in the produced Daikon log. Default is all.",                              
                     optg ""  ["llo"] (ReqArg ParamLLO "regex") 
                               "Extract low-level log data of functions that match the regex from the given compressed log file (.lox). The data will be produced in the Daikon-format, e.g. for oracles inference."
                         [opt1 "" ["lloMode"] (ReqArg (ParamLLOlevel . read) "int")
                                    "The mode of LLO conversion. Default is 1."  ,
                          opt1 "" ["lloChopAt"] (ReqArg (ParamLLOChopAt . read) "int")
                                    "Specifying where to chop-off input. Default is no-chop."
                         ]
                    ]
                    
-- For printing usage info                    
help  = putStrLn help_  
help_ = usageTInfo "USAGE: haslog --help, haslog options+ logfile"  optionTrees             
  
{-
main = do { args <- getArgs ; main_ args }

mainf args = main_ (words args)

main_ args = do {
   case args of
      ("--rstats" : fn : _)  ->  printStatistics fn
      ("--compress" : fn : _)  ->  compressThenSaveRawLog fn
      ("--toxml" : fn : _)     ->  saveAsXML fn
      ("--htoxml" : fn :_)     ->  filterAppEventsThenSaveAsXML fn
      ("--daikon" : fn :_)     ->  saveAsDaikonLog fn
      ("--llo" : funName: level : fn : _) ->  saveAsLLODaikonLog funName (read level) fn
      ("--help" : _)           ->  putStrLn (concat . intersperse "\n" $ infos)
      _  -> putStrLn "** Error:options not recognized."
   }
   where
   infos = ["--rstats <name of file containing raw log>  (print raw log statistics)",
            "--compress <name of file containing raw log>  (save raw log in compressed form)",
            "--toxml  <name of file containing compressed log> (convert compressed log to XML)",
            "--htoxml <name of file containing compressed log> (convert only high level events in a compressed log to XML)",
            "--daikon <name of file containing compressed log> (convert the log to daikon-format)",
            "--llo <qualified-function-name> <int> <name of file containing compressed log> (produce a daikon log containing control flows info, for oracle inference)",
            "--help ... obvious :)" ]

-}