-- | Commandline options
module Options where

import System.Console.GetOpt
import System.Environment
import System.FilePath
import System.IO
import System.Directory
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as L
import LogPointsLib
import Control.DeepSeq

--
-- Here we just define a record type for specifying all sorts of options for
-- the instrumenter. We do not define their semantics here. Some AG semantic
-- functions may require an option-record to be passed (and adjust its their
-- working according to some options in the record). Many of the options are
-- actually used by top level tools that uses this library.
--

data Options
  = Options { optSourceFile      :: !FilePath
            , optOutputFile      :: !(Maybe FilePath)
            , optInstrument      :: !Bool
            , optVerbose         :: !Bool
            , optSaveAst         :: !(Maybe String)
            , optDumpSym         :: !Bool
            , optDumpTypes       :: !Bool
            , optSaveCFG         :: !(Maybe String)   -- option to save the CFGs in a file                        
            , optGenLib          :: !(Maybe String)
            , optGenInh          :: !(Maybe FilePath)
            , optGenSymLib       :: !(Maybe FilePath)
            , optInjectAbc       :: ![FilePath]
            , optInjectBefore    :: !Bool
            , optInjectRefl      :: !Bool
            , optInjectSerial    :: !Bool
            , optEnv             :: ![FilePath]
            , optDumpStats       :: !Bool
            , optSwfCheck        :: !Bool
            , optPrettyLabs      :: !Bool
            , optConfigs         :: ![ByteString]  -- this option is for loading configuration from a file;
                                                   -- for now unsuported.
            , optExportSym       :: !(Maybe FilePath)
            , optImportSym       :: ![FilePath]
            , optRemoveDeadCode  :: !Bool
            , optLogPointsSpecs  :: ![LogPointSpec]  -- log-points specifications for log-injector
            }

--
-- these are basic ABCI options
--
basicOpts :: [OptDescr (Options -> IO Options)]
basicOpts = [ Option "o" ["output"]        (ReqArg oOutput "file")    "output .swf file"
       , Option "v" ["verbose"]       (NoArg oVerbose)           "verbose output"
       , Option ""  ["save-ast"]      (ReqArg oSaveAst "file")   "save pretty printed AST of the swf"
       , Option ""  ["dump-sym"]      (NoArg oDumpSym)           "dump symbol table"
       , Option ""  ["dump-types"]    (NoArg oDumpTypes)         "dump type graph"
-- specific abciLog options:       
--     , Option ""  ["save-cfg"]      (ReqArg oSaveCFG "file")   "save (final) control flow graphs"
       , Option ""  ["gen-lib"]       (ReqArg oGenLib "name")    "generate instrumentation support"
       , Option ""  ["gen-inh"]       (ReqArg oGenInh "file")    "generate inheritance file"
       , Option ""  ["gen-sym-lib"]   (ReqArg oGenSymLib "file") "generate symbol swc library"
       , Option "i" ["instrument"]    (NoArg oInstrument)        "run the instrumentation pipeline"
       , Option ""  ["inject-abc"]    (ReqArg oInjectAbc "file") "inject this .swc file"
       , Option ""  ["inject-before"] (NoArg oInjectBefore)      "inject the .swc before the first code tag instead of after last"
       , Option ""  ["inject-refl"]   (NoArg oInjectRefl)        "inject reflection information"
       , Option ""  ["inject-serial"] (NoArg oInjectSerial)      "inject serializer"
       , Option ""  ["env"]           (ReqArg oEnv "env")        "currently has no effect"
       , Option "s" ["dump-stats"]    (NoArg oDumpStats)         "dump statistics of .swf file"
       , Option "c" ["check"]         (NoArg oSwfCheck)          "check the structure of an .swf file"
       , Option ""  ["prettylabs"]    (NoArg oPrettyLabs)        "include instruction labels in pp output"     
-- config option is for now unsupported:       
--     , Option ""  ["config"]        (ReqArg oConfig "file")    "load a utf8 file (future use)"
       , Option ""  ["export-sym"]    (ReqArg oExportSym "file") "Exports the symbol information to file"
       , Option ""  ["import-sym"]    (ReqArg oImportSym "file") "Imports the symbol information from file"
       , Option ""  ["no-dead-code"]  (NoArg oNoDeadCode)        "Disables dead code removal"
-- specific abciLog option:       
--     , Option ""  ["log-points-spec-file"] (ReqArg oLogPointSpec "file")  "This file specifies the log-points specification"
       ]


--
-- These functions are invoked for each parsed commandline argument, and update the
-- options record. Some commandline arguments take parameters, which are passed as
-- first argument.
--

oOutput :: FilePath -> Options -> IO Options
oOutput s o = return (o { optOutputFile = Just s })

oVerbose :: Options -> IO Options
oVerbose o = return (o { optVerbose = True })

oInstrument :: Options -> IO Options
oInstrument o = return (o { optInstrument = True })

oSaveAst :: FilePath -> Options -> IO Options
oSaveAst f o = return (o { optSaveAst = Just f })

oDumpSym :: Options -> IO Options
oDumpSym o = return (o { optDumpSym = True })

oDumpTypes :: Options -> IO Options
oDumpTypes o = return (o { optDumpTypes = True })

oDumpStats :: Options -> IO Options
oDumpStats o = return (o { optDumpStats = True })

oSwfCheck :: Options -> IO Options
oSwfCheck o = return (o { optSwfCheck = True })

oGenLib :: String -> Options -> IO Options
oGenLib n o = return (o { optGenLib = Just n })

oGenInh :: FilePath -> Options -> IO Options
oGenInh f o = return (o { optGenInh = Just f })

oGenSymLib :: FilePath -> Options -> IO Options
oGenSymLib f o = return (o { optGenSymLib = Just f })

oInjectBefore :: Options -> IO Options
oInjectBefore o = return (o { optInjectBefore = True })

oInjectAbc :: String -> Options -> IO Options
oInjectAbc s o = return (o { optInjectAbc = splitSearchPath s })

oInjectRefl :: Options -> IO Options
oInjectRefl o = return (o { optInjectRefl = True })

oInjectSerial :: Options -> IO Options
oInjectSerial o = return (o { optInjectSerial = True })

oImportSym :: FilePath -> Options -> IO Options
oImportSym f o = return (o { optImportSym = f : optImportSym o })

oExportSym :: FilePath -> Options -> IO Options
oExportSym f o = return (o { optExportSym = Just f })

oEnv :: String -> Options -> IO Options
oEnv fs o = return (o { optEnv = splitSearchPath fs })

oPrettyLabs :: Options -> IO Options
oPrettyLabs o = return (o { optPrettyLabs = True })

oNoDeadCode :: Options -> IO Options
oNoDeadCode o = return (o { optRemoveDeadCode = False })

{-
oConfig :: FilePath -> Options -> IO Options
oConfig f o = do
  inp <- L.readFile f
  let s = L.copy inp
  seq (L.all $ const True) (return ())  -- ensures that the entire string is read in
  return (o { optConfigs = s : optConfigs o })
-}
  
defaultOpts :: Options
defaultOpts = Options { optSourceFile = "", optOutputFile = Nothing, optVerbose = False
                      , optInstrument = False, optSaveAst = Nothing, optDumpSym = False
                      , optInjectAbc = [], optGenLib = Nothing, optEnv = []
                      , optInjectRefl = False, optDumpStats = False, optSwfCheck = False
                      , optPrettyLabs = False, optInjectSerial = False, optConfigs = []
                      , optExportSym = Nothing, optImportSym = [], optDumpTypes = False
                      , optSaveCFG = Nothing 
                      , optGenInh = Nothing, optGenSymLib = Nothing 
                      , optRemoveDeadCode = True, optInjectBefore = False
                      , optLogPointsSpecs = []
                      }

commandlineArgs :: String -> [OptDescr (Options -> IO Options)] -> IO Options
commandlineArgs toolName setOfAllOptions
  = do args <- getArgs
       let usage = usageInfo ("Usage: " ++ toolName ++ " <option ...> <abc-file> ...") setOfAllOptions
       case getOpt Permute setOfAllOptions args of
         (actions, args', []) | null args' -> do hPutStrLn stderr ("No ABC source file specified.\n" ++ usage)
                                                 foldl (>>=) (return defaultOpts) actions
                              | otherwise  -> foldl (>>=) (return $ patch $ defaultOpts { optSourceFile = head args' }) actions
         (_, _, errs)                      -> do hPutStrLn stderr (unlines errs ++ "\n" ++ usage)
                                                 return defaultOpts
  where
    patch o = if optSourceFile o /= ""
              then o { optOutputFile = Just $ replaceExtension (optSourceFile o) ".out" }
              else o
