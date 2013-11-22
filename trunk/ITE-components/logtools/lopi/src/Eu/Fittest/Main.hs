-- lopi main file
module Main (main) where

-- import System.IO
import Control.Monad
import Eu.Fittest.Options
-- import System.FilePath
import Eu.Fittest.RPInferencer (inferConAlgRRules)
import Eu.Fittest.Data
import Data.List (groupBy, nub, (\\), intersperse)
import qualified Eu.Fittest.Pretty.Events as PPEvt
-- import qualified Eu.Fittest.Pretty.HOL as PPHol
import Text.PrettyPrint.HughesPJ (render)
-- import Data.Maybe
import Eu.Fittest.Fittest2SimpleLogParser 
import Eu.Fittest.WitnessSetParser
import Eu.Fittest.EventRewriting
import Eu.Fittest.Filter
import Eu.Fittest.Abstraction
import Eu.Fittest.CrossCheck
import System.FilePath.Posix
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
-- import Eu.Fittest.EventRewriting


main :: IO ()
main = do
  opts <- commandlineArgs
  
  when (optInference opts) $ do
    let inFiles = optInLogs opts
    mapM_ convFittest2SimpleLog inFiles
    inLogs <- mapM loadSimpleLog inFiles
    let evts = concatMap getEventList inLogs
        abst = case optAbsLogs opts of
          Nothing   -> idAbs
          Just absF | null absF -> idAbs
                    | otherwise -> readAF absF
        inLogsA = map (runAbsFun abst) inLogs               
        filter  = case optFilterRules opts of
          Nothing       -> filterZero
          Just filtPred | null filtPred -> filterZero
                        | otherwise     -> readCP filtPred
        wSet  = filterWitnesses (Filter filter) 
                $ concatWS
                $ map (inferConAlgRRules (Filter doesNotFilter) evts) inLogsA
        wSetStr = render $ PPEvt.ppWSet wSet
        outFile =  optRewRules opts
    writeFile outFile wSetStr

  when (optReduction opts) $ do
    let inFiles = optInLogs opts
        inRules = optRewRules opts
    mapM_ convFittest2SimpleLog inFiles
    inLogs <- mapM loadSimpleLog inFiles
    rs     <- readFile inRules >>= \x -> return $ map getRewRulPat $ getWitnesses x
    let evtLogs     = map (map event) inLogs
        rew_evtLogs = map (show . runRewrite rs) evtLogs
        outFiles    = case optOutLogs opts of
          Nothing   -> map (flip replaceExtension "rlog") inFiles
          Just outf | null outf -> map (flip replaceExtension "rlog") inFiles
                    | otherwise -> outf
    zipWithM_ writeFile outFiles rew_evtLogs
      
    -- need to describe oColRewStat option
      
  when (optCrossCheck opts) $ do
    let iargs = optCrossCheckRules opts
        infArgs@(inp1:inp2:outp:_) = iargs
    if (length infArgs /= 3)
      then fail $ "wrong number of arguments for I " ++
           (show $ length infArgs) ++ " instead of 3"
      else do rls1 <- readFile inp1 >>= return . getWitnesses
              rls2 <- readFile inp2 >>= return . getWitnesses
              writeFile outp $ concatMap (\x -> show x ++ "\n") $ crossCheck rls1 rls2
  
{-                  
main = do
  opts <- commandlineArgs
  -- info
  let source = optSourceLog opts
  when (source /= "") $ do
    convFittest2SimpleLog source
    log    <- loadSimpleLog source
    let evts    = getEventList log
        -- for now all inferred rules are returned
        -- inferConAlgRRules cann't handle inference rew.rules from set of logs
        filter  = case optFilterRules opts of
          Nothing       -> filterZero
          Just filtPred -> readCP filtPred
        wSet    = inferConAlgRRules (Filter filter) evts log
        wSetStr = render $ PPEvt.ppWSet wSet
        output =  case optDumpRules opts of 
          Nothing   -> replaceExtension source ".pat"
          Just file -> file 
    writeFile output wSetStr
  case optIntersectRules opts of
    Nothing      -> return ()
    Just iargs   -> do let infArgs@(inp1:inp2:outp:_) = words iargs
                       if (length infArgs /= 3)
                         then fail $ "wrong number of arguments for I " ++
                              (show $ length infArgs) ++ " instead of 3"
                         else do rls1 <- readFile inp1 >>= return . getWitnesses
                                 rls2 <- readFile inp2 >>= return . getWitnesses
                                 writeFile outp $ concatMap (\x -> show x ++ "\n") (rls1 \\ rls2)
-}                                 
                               
                               
-- if optEvtRewOnly opts
--       then 
--         case optOutputLog opts of
--           Nothing   -> return ()
--           -- resulting conversion still requires some changes because it produces 
--           -- the reduced log in simple log format insted of the fiitest one.
--           Just file -> writeFile file $ show $ runRewrite (map event log) rs
--       else undefined 
                        

{-
acceptWitnessSet :: FilePath -> IO [RewRulPat]
acceptWitnessSet fp = do
  putStrLn $ "Check out inferred rules in " ++ fp ++ 
             " and press [y] if you have finished."
  ws   <- readFile fp                 
  answ <- getLine 
  if answ == "y"
    then return $ getWitnesses ws
    else acceptWitnessSet fp
-}
  
              
         