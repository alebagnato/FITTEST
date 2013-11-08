module Eu.Fittest.Options where

import System.Console.GetOpt
import System.Environment
import System.FilePath
import System.IO
-- import System.Directory
-- import Data.ByteString.Lazy(ByteString)
-- import qualified Data.ByteString.Lazy as L
-- import LogPointsLib
-- import Control.DeepSeq
-- import Control.Monad
import Debug.Trace

data ToolType = Infer | Rewrite | Compare

data Options = 
  Options { optInference            :: Bool
          , optReduction            :: Bool
          , optCrossCheck           :: Bool  
          , optInLogs               :: [FilePath]
          , optOutLogs              :: Maybe [FilePath]
          , optRewRules             :: FilePath
          , optFilterRules          :: Maybe String
          , optAbsLogs              :: Maybe String  
          , optColRewStat           :: Bool
          , optCrossCheckRules      :: [FilePath]
          , optHtmlCrossCheckReport :: Bool  
          }
  
opts :: [OptDescr (Options -> IO Options)]
opts = [ Option "i" [] (NoArg oInference) "infer rewrite rules from logs"
       , Option "r" [] (NoArg oReduction) "apply reduction procedure to logs"
       , Option "c" [] (NoArg oCrossCheck) "cross check two set of rewrite rules"  
       , Option ""  ["in-logs"] (ReqArg oInLogs "files") "input log files"
       , Option ""  ["out-logs"] (ReqArg oOutLogs "files") "output log files"
       , Option ""  ["rew-rules"] (ReqArg oRewRules "file") "output rewrite rules"
       , Option "f" ["filter-rules"] (ReqArg oFilterRules "ffunc") "filter rewrite rules according to a predefined filter function"
       , Option "a" ["abs-logs"] (ReqArg oAbsLogs "afunc") "apply abstraction function afunc to the logs before inference"
       , Option "s" ["col-rew-stats"] (NoArg oColRewStat) "collect rewriting statistics"
       , Option ""  ["cross-check-rules"] (ReqArg oCrossCheckRules "inp1 inp2 out") "cross check two set of inferred rewrite rules against of each-other"
       ]

{-
oToolType :: String -> Options -> IO Options
oToolType s o = do
  case s of
    "i" -> return (o {optToolType = Infer})
    "r" -> return (o {optToolType = Rewrite})
    "c" -> return (o {optToolType = Compare})
    _   -> fail  ("no such an option"  ++ (usageInfo "Usage: toolname <option ...> <log file> ..." opts))
-}

oInference :: Options -> IO Options
oInference o = return (o {optInference = True})

oReduction :: Options -> IO Options
oReduction o = return (o {optReduction = True})

oCrossCheck :: Options -> IO Options
oCrossCheck o = return (o {optCrossCheck = True})

oInLogs :: FilePath -> Options -> IO Options
oInLogs s o = return (o {optInLogs = words s})

oOutLogs :: FilePath -> Options -> IO Options
oOutLogs s o = return (o {optOutLogs = Just $ words s})

oRewRules :: FilePath -> Options -> IO Options
oRewRules s o = return (o {optRewRules = s})

oFilterRules :: String -> Options -> IO Options
oFilterRules s o = return (o {optFilterRules = Just s})

oAbsLogs :: String -> Options -> IO Options
oAbsLogs s o = return (o {optAbsLogs = Just s})

oColRewStat :: Options -> IO Options
oColRewStat o = return (o {optColRewStat = True})

oCrossCheckRules :: String -> Options -> IO Options
oCrossCheckRules s o = return (o {optCrossCheckRules = words s})

defaultOpts :: Options
defaultOpts = 
  Options { optInference       = False
          , optReduction       = False
          , optCrossCheck      = False
          , optInLogs          = []
          , optOutLogs         = Nothing
          , optRewRules        = []
          , optFilterRules     = Nothing
          , optAbsLogs         = Nothing
          , optColRewStat      = False
          , optCrossCheckRules = []
          }
  
commandlineArgs :: IO Options
commandlineArgs = 
  do args <- getArgs
     let usage = usageInfo "Usage: toolname <option ...> <log file> ..." opts
     case getOpt Permute opts args of
       (op, nop, []) -> foldl (>>=) (return $ defaultOpts) op
       (_, _, errs)  -> do hPutStrLn stderr (unlines errs ++ "\n" ++ usage)
                           return defaultOpts

  -- (op, nop, []) | null nop  -> foldl (>>=) (return $ patch $ defaultOpts) op
  --               | otherwise -> foldl (>>=) (return $ patch $ defaultOpts {optSourceLog = head nop}) op
  -- where 
  --   patch o = if optSourceLog o /= ""
  --             then o { optOutputLog = Just $ replaceExtension (optSourceLog o) ".pat"}
  --             else o