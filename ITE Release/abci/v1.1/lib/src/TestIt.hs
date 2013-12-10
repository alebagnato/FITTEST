-- | Testing module.
--
--   Runs an instrumentation and pretty-prints the resulting program to a file
--
--   E.g.
--   lib$ cabal build
--   lib$ ghci -isrc -idist/build/ DemoTrans
--
--   *DemoTrans> :module +TestIt
--
--   *DemoTrans TestIt> runIt (Just demoSwf) "/Users/ariem/Downloads/HelloWorld.swf" "/tmp/pretty.txt"
--
--   or an alternative for the last command:
--   *DemoTrans TestIt> let source = "/Users/ariem/Downloads/HelloWorld.swf"
--   *DemoTrans TestIt> let dest = "/tmp/pretty.txt"
--   *DemoTrans TestIt> runIt (Just demoSwf) source dest
--

{-# LANGUAGE BangPatterns #-}
module TestIt(runIt,runIt__) where

import System.IO
import Control.Monad
import Parser.Parser
import qualified Pretty.AVMTree as PT
import qualified Pretty.AVMCode as PC 
import Options.Options
import qualified Data.ByteString.Lazy as L
import Util.ProgInfo
import ByteCode.ByteCode
import Options.GathSymbolTables
import Analysis.DeadCodeRemoval
import Pretty.AVMUtil
import Data.Monoid
import Util.ParamAnalysis
import Data.Maybe
import ByteCode.Consolidate
import Options.ForceAstEval
import Boogie.Boogie (swfToBoogie)
import Options.PPCFG (exportCFG)
-- import Options.XCFG
import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as C

type Injection = Options -> [SymbolTables] -> SwfFile -> SwfFile

-- | Give the injection function, and the source and destination file paths
runIt :: Maybe Injection -> FilePath -> FilePath -> IO ()
runIt !mbInj !source !dest = runIt__ myOptions mbInj source dest
   where
   myOptions = defaultOpts { optVerbose      = True
                           , optPrettyLabs   = True
                           , optExportBoogie = Just ""
                           }

runIt__ :: Options -> Maybe Injection -> FilePath -> FilePath -> IO ()
runIt__ !myOptions !mbInj !source !dest = do
  -- prepare options
  let !opts = myOptions { optSourceFile = source, optInstrument = isJust mbInj }

  -- get bytecode and remove dead code
  -- !swf <- parseAnyAsSwf source
  !swf <- parseAnyAsSwf source
          -- >>= return . removeDeadCode -- uncomment in case the dead code need to be removed as well
  hPutStrLn stderr "Parsed SWF."

  -- prepare symbol tables
  let !tblsInitial = symInfoSwf swf
      !swfTpInfo   = mconcat $ map toTypeInfo tblsInitial
  hPutStrLn stderr "Extracted symbol info."

  let !tpInfo = swfTpInfo
      !tpGr   = toTypeGr tpInfo
      !tbls   = map (\t -> t { tableTypeInfo = tpInfo, graphTypeInfo = tpGr }) tblsInitial
  -- when (isTpGrCyclic tpGr) (hPutStrLn stderr "warning: type inheritance graph is cyclic")
  hPutStrLn stderr "Computed type info."
  
  -- run injection
  let !swf1  | optInstrument opts = (fromJust mbInj) opts tbls swf
             | otherwise          = swf
      !() = forceAstEval swf1
  hPutStrLn stderr "Ran the instrumentation (if enabled)."
 
  let tbls' = symInfoSwf swf1
      !swf2  = consolidate swf1
  hPutStrLn stderr "Updated tables."
  

  -- dump CFG
  case optSaveCFG opts of
    Nothing   -> hPutStrLn stderr "CFG isn't outputted" >> return ()
    Just file | null file -> do hPutStrLn stderr "CFG is outputted"
                                L.writeFile (replaceExtension source ".dot")
                                  $ C.pack
                                  $ exportCFG opts tbls' emptyMethodDefInfos swf
                                  --cfg2dot opts tbls swf
              | otherwise -> do hPutStrLn stderr "CFG is outputted" 
                                L.writeFile file $ C.pack $
                                  exportCFG opts tbls' emptyMethodDefInfos swf
                                  -- cfg2dot opts tbls swf    
  
  if (source == dest)
    then hPutStrLn stderr "I almost overwrote your source file..."
    else do -- L.writeFile dest $ renderBytes $ PT.ppSwf opts tbls' emptyMethodDefInfos swf2
            L.writeFile (dest ++ ".out") $ PC.ppSwf opts swf2
  hPutStrLn stderr "Outputted the results."
  
  case optExportBoogie opts of
    Nothing   -> hPutStrLn stderr "swf isn't tanslated to boogie" >> return ()
    Just file | null file -> do hPutStrLn stderr "swf is tanslated to boogie"
                                L.writeFile (replaceExtension source ".bpl")
                                  $ renderBytes
                                  $ swfToBoogie opts tbls' swf
              | otherwise -> do hPutStrLn stderr "swf is tanslated to boogie" 
                                L.writeFile file
                                  $ renderBytes
                                  $ swfToBoogie opts tbls' swf
