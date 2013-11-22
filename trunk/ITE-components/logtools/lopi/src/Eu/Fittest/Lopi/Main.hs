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
import System.FilePath.Posix
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
-- import Eu.Fittest.EventRewriting


main :: IO ()
main = do
  opts <- commandlineArgs
  case optToolType opts of
    Infer -> do let inFiles = optInInferLogs opts
                mapM_ convFittest2SimpleLog inFiles
                inLogs      <- mapM loadSimpleLog inFiles
                let evts    = concatMap getEventList inLogs
                    filter  = case optFilterRules opts of
                      Nothing       -> filterZero
                      Just filtPred -> readCP filtPred
                    wSet    = filterWitnesses (Filter filter) 
                              $ concatWS
                              $ map (inferConAlgRRules (Filter doesNotFilter) evts) inLogs
                    wSetStr = render $ PPEvt.ppWSet wSet
                    outFile =  optOutRewRules opts
                writeFile outFile wSetStr
    Rewrite -> do let inFiles = optInRewLogs opts
                      inRules = optOutRewRules opts
                  mapM_ convFittest2SimpleLog inFiles
                  inLogs      <- mapM loadSimpleLog inFiles
                  rs          <- readFile inRules >>= \x -> return $ getWitnesses x
                  let evtLogs = map (map event) inLogs
                      rew_evtLogs = map (show . runRewrite rs) evtLogs
                      outFiles = case optOutRewLogs opts of
                        Nothing   -> map (flip replaceExtension "rlog") inFiles
                        Just outf -> outf
                  zipWithM_ writeFile outFiles rew_evtLogs
                  -- need to describe oColRewStat option
    Compare -> do let iargs = optIntersectRules opts
                      infArgs@(inp1:inp2:outp:_) = iargs
                  if (length infArgs /= 3)
                    then fail $ "wrong number of arguments for I " ++
                         (show $ length infArgs) ++ " instead of 3"
                    else do rls1 <- readFile inp1 >>= return . getWitnesses
                            rls2 <- readFile inp2 >>= return . getWitnesses
                            writeFile outp $ concatMap (\x -> show x ++ "\n") (rls1 \\ rls2)
  
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
  
              
         