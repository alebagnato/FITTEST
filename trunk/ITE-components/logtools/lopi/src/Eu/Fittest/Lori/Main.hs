-- lori main file

module Main (main) where

import System.IO
import Control.Monad
import Eu.Fittest.Options

import Eu.Fittest.RPInferencer (inferConAlgRRules)
import Eu.Fittest.Data
import Data.List (groupBy, nub, (\\), )
import qualified Eu.Fittest.Pretty.Events as PPEvt
import qualified Eu.Fittest.Pretty.HOL as PPHol
import Text.PrettyPrint.HughesPJ (render)
import Data.Maybe
import Eu.Fittest.Fittest2SimpleLogParser 
import Eu.Fittest.WitnessSetParser
import Eu.Fittest.Filter (filterWitnesses, Filter (..), defConfLevelPred)
import System.FilePath.Posix
import Debug.Trace
import Eu.Fittest.EventRewriting
import Data.Function

main :: IO ()
main = do
  opts <- commandlineArgs
  let source   = optSourceLog opts
      rules    = case optDumpRules opts of 
        Nothing   -> replaceExtension source ".pat"
        Just file -> file   
      output   = case optOutputLog opts of     
        Nothing   -> replaceBaseName source (takeBaseName source ++ "__")
        Just file -> file  
  convFittest2SimpleLog source
  logs <- loadSimpleLog source      
  let evts = map event logs
  rs   <- readFile rules >>= \x -> return $ getWitnesses x
  let rew_evts = runRewrite rs evts
  if (optColRewStat opts)
    then do let fp           = replaceExtension source ".rstat"
                sts          = collectAllRewStats evts rew_evts
                (mx, mn, av) = collectMaxMinAvStats sts
                stsData      = print_collectRewStats sts
                (avLO, avLR) = collectAvLenStats sts
            withFile fp WriteMode (\hld -> mapM_ (hPutStrLn hld) stsData)
            appendFile fp ("average reduction is " ++ show av ++ "\n")
            appendFile fp ("maximum reduction is " ++ show mx ++ "\n")
            appendFile fp ("minimum reduction is " ++ show mn ++ "\n")
            appendFile fp ("average length of original log is " ++ show avLO ++ "\n")
            appendFile fp ("average length of reduced log is "  ++ show avLR ++ "\n")
    else return ()
  writeFile output $ show rew_evts
        
  
collectAllRewStats :: [[CEvent]] -> [[CEvent]] -> [(Float, Float, Float)]
collectAllRewStats = zipWith getStat where
  getStat x y = 
    let l1 = fromIntegral $ length x
        l2 = fromIntegral $ length y
        r  = l1 / l2 
    in  (l1, l2, r)   
        
print_collectRewStats :: [(Float, Float, Float)] -> [String]
print_collectRewStats = map printAllData
  where
    printAllData (x, y, r) = show x ++ " " ++ show y ++ " " ++ show r
    
collectMaxMinAvStats :: [(Float, Float, Float)] -> (Float, Float, Float)
collectMaxMinAvStats sts = let rs = map (\(_,_,r) -> r) sts
                         in (maximum rs, minimum  rs, (sum rs) / (fromIntegral $ length rs))
                            
collectAvLenStats :: [(Float, Float, Float)] -> (Float, Float)
collectAvLenStats sts =  (average [ x | (x, _, _) <- sts], average [y | (_, y, _) <- sts])
  where
    average :: [Float] -> Float
    average as = sum as / (fromIntegral $ length as)


                     
