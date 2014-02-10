module Eu.Fittest.CrossCheck where

import Eu.Fittest.Data
import Data.List
import Data.Maybe

data CrossCheckStatus = P | N | U
                      deriving Show

-- | the crossCheck function follows given comparison principle:
-- | ws1 is a set of rules that has be accepted as valid for the
-- | initial version of the system; whereas ws2 is a set of rules
-- | inferred from the logs produced by the modified version of
-- | the same system. The purpose of the cross checking consists in
-- | justifying the rules in ws1 are still hold by looking at ws2.
-- | A rule r in ws1 is still valid if it doesn't have a negative
-- | witness in ws2. If r doesn't occur in ws2 we can't make a certain
-- | judgment and report it as unconfirmed.
crossCheck :: Witnesses -> Witnesses -> [(Witness, CrossCheckStatus)]
crossCheck ws1 ws2 = mapMaybe (\x -> elemIndex x ws2 >>= return . setStatus . (ws2!!)) ws1
  where
    setStatus w = case getWitnessCount w of
      (p, n) | n > 0  -> (w, N)
             | p == 0 -> (w, U)
             | p > 0  -> (w, P)
      _               -> error "something wrong in the `setStatus` function"
      
    