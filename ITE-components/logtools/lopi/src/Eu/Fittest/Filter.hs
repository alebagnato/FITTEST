module Eu.Fittest.Filter 
       ( filterWitnesses
       , filterZero
       , filterNegative
       , doesNotFilter
       , defConfLevelPred
       , ConfLevelPred
       , Filter (..)
       , readCP
       ) where

--import Eu.Fittest.RPInferencer 
import Control.Monad (foldM)
import Eu.Fittest.Data

type ConfLevelPred = (Int, Int) -> Bool

readCP :: String -> ConfLevelPred
readCP f = case f of
  "defConfLevelPred" -> defConfLevelPred
  "filterNegative"   -> filterNegative
  "filterZero"       -> filterZero
  "doesNotFilter"    -> doesNotFilter

type ConfLevel = Float

-- | This is confidence level used by default.
defConfLevel :: Float
defConfLevel = 0.99

-- | This function is conf.level function that used in computations by default.
-- | We can tune this function with the length of State data type
defConfLevelPred :: ConfLevelPred
defConfLevelPred (p,n) | n == 0    = (1 - (0.5)^p) > defConfLevel
                      | otherwise = False

filterNegative :: ConfLevelPred
filterNegative (_,n) | n == 0    = True
                     | otherwise = False

filterZero :: ConfLevelPred
filterZero (p,n) | p /= 0 && n == 0 = True
                 | otherwise        = False

doesNotFilter :: ConfLevelPred 
doesNotFilter (_,_) = True

data Filter = Filter ConfLevelPred

filterWitnesses :: Filter -> [Witness] -> [Witness]
filterWitnesses (Filter clPred) wits = 
  foldl foldWits [] wits
    where
     foldWits :: [Witness] -> Witness -> [Witness]
     foldWits res wit@(WitnessC rn r (p,n)) = if (clPred (p,n))
                                              then wit:res
                                              else res
