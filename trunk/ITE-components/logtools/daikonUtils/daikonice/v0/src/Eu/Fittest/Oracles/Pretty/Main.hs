{- 

Author: Wishnu Prasetya

Copyright 2013 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 

   This module provides functions to convert Daikon oracle reports to nice HTML
   for viewing.
  
-}

module Main (main) where

import Eu.Fittest.Oracles.Ast.OrcsDataType
import Eu.Fittest.Oracles.Parser.DaikonParser
import Eu.Fittest.Oracles.Pretty.HTMLPrettyPrinter 

import Eu.Fittest.Oracles.Options

import Debug.Trace

nocheckDaikonice1 :: String -> String -> Int -> IO ()
nocheckDaikonice1 f = nocheckDaikonice [f]

nocheckDaikonice :: [String] -> String -> Int -> IO ()
nocheckDaikonice daikonReportFiles = daikonice daikonReportFiles [] 
   
   
-- To pretty print a list of oracle-report files.
-- The function also read from a list of violations-reports to mark the oracles accordingly.
-- You also need to specify the name of output html, and the minimum number of witness 
-- to be used to filter the oracles.
daikonice :: [String] -> [String] -> String -> Int -> IO ()
daikonice daikonReportFiles violationsFiles htmlOutputFile minimumNumberOfWitnesses = do {
     orcsuite <- parseDaikonOrcsReports daikonReportFiles isNotWantedOrc ;
     violations <- parseViolationsFromFiles violationsFiles ;
     suite <- (if null violations then return orcsuite
                                  else return (markOrcsSuiteWithViolations violations orcsuite)) ;
     orcsSuite2Html htmlOutputFile suite ;
     trace (">>>" ++ show (length violations)) (return ()) ;
  }
  where
  -- drop "has only one value" type of oracle; this is not really incompatible with 
  -- oracle checking our approach
  isNotWantedOrc (Pre (O_isConstant _) , _, _) = True
  isNotWantedOrc (Post (O_isConstant _), _, _) = True
  -- drop oracle that does not have enough witnesses:
  isNotWantedOrc (_,Just k,_)  = k < minimumNumberOfWitnesses
  isNotWantedOrc (_,Nothing,_) = minimumNumberOfWitnesses > 0  -- only show this if no witness is asked


main :: IO ()
main = do
  opts <- commandlineArgs
  let oreports = optOracleReports opts
      vreports = optViolationReports opts
      output   = optOutputHtml opts
      min      = optMinWitnessFilter opts
  daikonice oreports vreports output min
