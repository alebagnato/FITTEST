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
import Parser
import qualified PrettyTree as PT
import qualified PrettyCode as PC
import Options
import qualified Data.ByteString.Lazy as L
import ProgInfo
import ByteCode
import GathSymbolTables
import DeadCodeRemoval
import PrettyUtil
import Data.Monoid
import ParamAnalysis
import Data.Maybe
import Consolidate
import ForceAstEval

type Injection = Options -> [SymbolTables] -> SwfFile -> SwfFile

-- | Give the injection function, and the source and destination file paths
runIt :: Maybe Injection -> FilePath -> FilePath -> IO ()
runIt !mbInj !source !dest = runIt__ myOptions mbInj source dest
   where
   myOptions = defaultOpts { optVerbose = True, optPrettyLabs = True }
 

runIt__ :: Options -> Maybe Injection -> FilePath -> FilePath -> IO ()
runIt__ !myOptions !mbInj !source !dest = do
  -- prepare options
  let !opts = myOptions { optSourceFile = source, optInstrument = isJust mbInj }

  -- get bytecode and remove dead code
  -- !swf <- parseAnyAsSwf source
  !swf <- parseAnyAsSwf source >>= return . removeDeadCode
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

  if (source == dest)
    then hPutStrLn stderr "I almost overwrote your source file..."
    else do -- L.writeFile dest $ renderBytes $ PT.ppSwf opts tbls' emptyMethodDefInfos swf2
            L.writeFile (dest ++ ".out") $ PC.ppSwf opts swf2
  hPutStrLn stderr "Outputted the results."
