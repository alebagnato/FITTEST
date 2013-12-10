module Eu.Fittest.Oracles.Options where

import System.Console.GetOpt
import System.Environment
-- import System.FilePath
import System.IO

data Options =
  Options { optOracleReports    :: [FilePath]
          , optViolationReports :: [FilePath]
          , optOutputHtml       :: FilePath
          , optMinWitnessFilter :: Int  
          }

opts :: [OptDescr (Options -> IO Options)]
opts = [ Option "r" ["oracles"] (ReqArg oOracleReports "oracles") "list of oracle reports"
       , Option "v" ["violations"] (ReqArg oViolationReports "violations") "list of violation reports"
       , Option "o" ["output"] (ReqArg oOutputHtml "html") "output html file"
       , Option "n" ["witness-number"] (ReqArg oMinWitnessFilter "number") "minimum number of witness to be used to filter the oracles" 
       ]

oOracleReports :: String -> Options -> IO Options
oOracleReports fs o = return (o {optOracleReports = words fs})

oViolationReports :: String -> Options -> IO Options
oViolationReports fs o = return (o {optViolationReports = words fs})

oOutputHtml :: String -> Options -> IO Options
oOutputHtml f o = return (o {optOutputHtml = f})

oMinWitnessFilter :: String -> Options -> IO Options
oMinWitnessFilter f o = return (o {optMinWitnessFilter = (read f :: Int)})

defaultOpts :: Options
defaultOpts = 
  Options { optMinWitnessFilter = 0} 

commandlineArgs :: IO Options
commandlineArgs = 
  do args <- getArgs
     let usage = usageInfo "Usage: toolname <option ...> <log file> ..." opts
     case getOpt Permute opts args of
       (op, nop, []) -> foldl (>>=) (return $ defaultOpts) op
       (_, _, errs)  -> do hPutStrLn stderr (unlines errs ++ "\n" ++ usage)
                           return defaultOpts


       