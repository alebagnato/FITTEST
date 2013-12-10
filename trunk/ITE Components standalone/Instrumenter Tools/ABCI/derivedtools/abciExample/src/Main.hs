-- | Instrumenter entry point
--
-- This main-file is a template to build a AS-bytecode instrumentation
-- tool on top ASIL (AS Insrtumentation Library). When compiled, this
-- file will produce an executable. However, the instrumentation is
-- set to just do nothing. The tool does get other auxilliary
-- functionalities along, such as printing some basic 
-- statistics of the target 
-- bytecode, and pretty printing the byte code. To build a derived
-- instrumentation tool, you need to replace the empty injection with
-- your own injection function. You will get the auxiliarry 
-- functionalities along.
-- 
--


-- Developers notes:
-- * If an error message is printed:
--     > hClose: illegal operation (handle is finalized)
--   This likely means that there is a hard cycle ("blackhole") in the code.
--

{-# LANGUAGE BangPatterns #-}
module Main(main) where

import System.IO
import Control.Monad
import Parser
import qualified PrettyCode as PC
import qualified PrettyTree as PT
import Options
import System.Console.GetOpt
import qualified Data.ByteString.Lazy as B
import ProgInfo
import ByteCode
import TrfInjectAbc
import TrfInjectRefl
import TrfInjectSerializable
import GathSymbolTables
import PrettyUtil
import Data.Monoid
import ExtractAbc
import SwfFileCheck
import ParamAnalysis
import Spec
import Stats
import GenInh
import GenSymLib
import DeadCodeRemoval
import Consolidate
import ForceAstEval

-- | Describes different types of outputs
data Result
  = ResSwf !SwfFile
  | ResDoc !Doc
  | ResStr !B.ByteString
  | ResNothing

-- defining the set of all available options for this top-level tool;
-- for this example tool we just use basic sets defined in Options:
myAllOpts :: [OptDescr (Options -> IO Options)]
myAllOpts = basicOpts 
  
-- | Takes an swf file as parameter and processes it depending on the commandline options.
main :: IO ()
main = do
  !opts <- commandlineArgs "abciExample" myAllOpts
  info opts "starting"
  let !source = optSourceFile opts
  when (source /= "") $ do
    when (optSwfCheck opts) $ do
      info opts "checking swf"
      checkSwfFile source

    info opts "parsing swf"
    !swf <- parseAnyAsSwf source >>=
               if optRemoveDeadCode opts
               then return . removeDeadCode
               else return
    let !() = forceAstEval swf

    info opts "preparing symbol tables"
    let !tblsInitial = symInfoSwf swf
        !swfTpInfo   = mconcat $ map toTypeInfo tblsInitial
    !envTpInfoL <- mapM loadTypeInfos (optImportSym opts)
    let !envTpInfos = mconcat envTpInfoL
        !tpInfo = mappend swfTpInfo envTpInfos
        !tpGr   = toTypeGr tpInfo
        !tbls   = map (\t -> t { tableTypeInfo = tpInfo, graphTypeInfo = tpGr }) tblsInitial
    -- when (isTpGrCyclic tpGr) (hPutStrLn stderr "warning: type inheritance graph is cyclic")
    info opts "prepared symbol tables"

    info opts "analyzing stack parameters"
    let !defSites = analyzeDefsSwf opts swf
    info opts "analyzed stack parameters"

    when (optDumpTypes opts) (hPutStrLn stdout $ show tpGr)
    maybe (return ()) (saveTypeInfo swfTpInfo) (optExportSym opts)
    when (optDumpSym opts) (hPutStrLn stdout $ show tbls)

    case (optSaveAst opts) of
      Nothing -> return ()
      Just f  -> do
                 info opts "Saving AST"
                 withFile f  WriteMode . flip B.hPut . renderBytes $ PT.ppSwf opts tbls defSites swf

    when (optDumpStats opts) $ do
      info opts "dump AST statistics"
      hPutStrLn stdout (unlines (map show (stats swf)))

    case optGenInh opts of
      Nothing   -> return ()
      Just file -> do
        info opts "generating inheritance file"
        generateInheritanceFile tpGr tbls file

    case optGenSymLib opts of
      Nothing   -> return ()
      Just file -> do
        info opts "generating symbol information library"
        generateSymbolLibrary opts envTpInfos file tbls swf


    info opts "process swf file"
    !res <- process opts tbls tpInfo swf

    info opts ("output result to " ++ maybe "stdout" id (optOutputFile opts))
    let !outp = case optOutputFile opts of
                  Nothing   -> B.hPut stdout
                  Just dest -> withFile dest WriteMode . flip B.hPut
    case res of
      ResSwf !swf1 -> do info opts "Consolidating SWF"
                         let !swf2 = consolidate swf1
                             !()   = forceAstEval swf2
                         info opts "Writing result"
                         outp $ PC.ppSwf opts swf2
      ResDoc doc  -> outp $ renderBytes doc
      ResStr str  -> outp str
      ResNothing  -> return ()

-- | Outputs a message if the verbose option is enabled
info :: Options -> String -> IO ()
info !opts !msg
  | optVerbose opts = hPutStrLn stderr ("asic: " ++ msg)
  | otherwise       = return ()

-- | Processes the swf file
process :: Options -> [SymbolTables] -> TypeInfo -> SwfFile -> IO Result
process !opts !tbls !tpInfo !file = pipeline file
  [specTrans opts tbls, injRefl opts tbls, injSerial opts tbls, injAbc opts]

-- | Sequences transformations on the swf file
pipeline :: SwfFile -> [SwfFile -> IO (Maybe SwfFile)] -> IO Result
pipeline !file = fmap wrap . foldl (>>=) (return $! Left file) . map act where
  wrap = either (const ResNothing) ResSwf
  act !f !inp = fmap (maybe inp Right) $! f (either id id inp)

-- | Wrapper around the reflection injection
injRefl :: Options -> [SymbolTables] -> SwfFile -> IO (Maybe SwfFile)
injRefl !opts !tbls !file
  | optInjectRefl opts = return $! Just $! injectReflSwf tbls file
  | otherwise          = return Nothing

-- | Wrapper around the serializable injection
injSerial :: Options -> [SymbolTables] -> SwfFile -> IO (Maybe SwfFile)
injSerial !opts !tbls !file
  | optInjectSerial opts = return $! Just $! injectSerializable tbls file
  | otherwise            = return Nothing

-- | Wrapper around the abc injection
injAbc :: Options -> SwfFile -> IO (Maybe SwfFile)
injAbc !opts !file = case optInjectAbc opts of
    []    -> return Nothing
    paths -> do
      !swfs <- mapM parseAnyAsSwf paths
      return $! Just $! foldl inj file $ concatMap extractAbcFiles swfs
  where inj = flip (injectAbc (optInjectBefore opts) "asic-injected-code")
