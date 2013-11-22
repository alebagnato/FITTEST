{-# LANGUAGE ParallelListComp, TransformListComp, GADTs #-}

module Eu.Fittest.RPInferencer 
       ( skip
       , zero
       , idemp
       , ignore
       , com
       , mkWSet
       , checkRRPat
       , countPats 
       , returnNegWit
       , subseqsOfLog -- hide later on
       , inferConAlgRRules
       ) where

import Eu.Fittest.Data
import Eu.Fittest.Filter
import Data.IntMap (IntMap) 
import qualified Data.IntMap as IntMap
import Debug.Trace
import Data.Maybe
import GHC.Exts
import Data.List (groupBy, nub, (\\))
-- import qualified PrettyEvents as PPEvt
--import qualified PrettyHOL as PPHol
import Eu.Fittest.Substrings
import Text.PrettyPrint.HughesPJ (render)
import Safe 

-- ====================================================      
--        Interfer patterns using trie data structure 
--        (an efficient version of the algorithm)    
-- ==================================================== 

mapFromLog :: Log -> IntMap LogEntry
mapFromLog log = IntMap.fromList $ zip [0..] log 

subseqsOfLog :: Int -> Log -> [(Log,[Int])]
subseqsOfLog maxLength log = subseqsOfSeq maxLength log  

-- | This function is used to construct witness set for a given rewrite rule.
-- | It takes 3 arguments: a rewrite rule, a log and the list of all sublogs
-- | with its starting possitions. 
mkWSet :: RRPattern RewRulPat t -> Log -> [(Log, [Int])] -> [(Log,Log)]
mkWSet pat log substrs = 
  case pat of
    SkipLike _ -> [(lhs, init lhs) | lhs <- lsegs]
    _          -> [(lhs, rhs) | lhs <- lsegs
                              , rhs <- rsegs  
                              , (state $ headNote "empty lhs" lhs) 
                                == 
                                (state $ headNote "empty rhs" rhs)
                              , checkSegsWithPat' (pl :~: pr)  (tail lhs) (tail rhs)
                              ]
  where
    pl :~: pr = getRule pat
    plL = length pl
    prL = length pr
    logSubstr =  substrs 
    lSubPos = snd $ headDef ([],[]) [ (lhs,lpos) | (lhs,lpos) <- logSubstr
                                                 , map (conEName.event) lhs 
                                                   == 
                                                   map symEName pl
                                                 ]
    rSubPos = snd $ headDef ([],[]) [ (rhs,rpos) | (rhs,rpos) <- logSubstr
                                                 , map (conEName.event) rhs 
                                                   == 
                                                   map symEName pr
                                                 ]
    takeSubstr ::  [a] -> Int -> Int -> [a]
    takeSubstr as l i | l + i <= length as = take l $ drop i as
                      | otherwise         = []
    lsegs = filter (/=[]) $ map (takeSubstr log (plL+1) . flip (-) 1) lSubPos
    rsegs = filter (/=[]) $ map (takeSubstr log (prL+1) . flip (-) 1) rSubPos
    
checkSegsWithPat' :: RewRulPat -> Log -> Log -> Bool
checkSegsWithPat' p@(lpat :~: rpat) lseg rseg = 
  let levs   = map event lseg  
      revs   = map event rseg
      symMap = pat2IntMap p
      conMap = cevents2IntMap (levs ++ revs)
      conMapLen = IntMap.size conMap - 1
      symMapLen = IntMap.size symMap - 1
      addConstr i b = 
        let (map_before, Just v, map_after) = IntMap.splitLookup i symMap 
            index = findWithDefaultValue i v map_before 
        in b && (conMap IntMap.! i == conMap IntMap.! index)
  in (foldr addConstr True [0..(min conMapLen symMapLen)])

findWithDefaultValue :: Eq a => Int -> a -> IntMap a -> Int
findWithDefaultValue def v m = let r = [k | (k, v') <- (IntMap.toList m), v == v']
                               in case r of
                                   [] -> def
                                   _  -> head r
                                    
pat2IntMap :: RewRulPat -> IntMap String
pat2IntMap (lp :~: rp) = snd $ foldr foldPat (0, IntMap.empty) (lp ++ rp)
  where
    foldPat :: SEvent -> (Int, IntMap String) -> (Int, IntMap String)
    foldPat (SEvent en eargs) (i, map) = (i+2, IntMap.insert (i+1) (symEvtArgs eargs) (IntMap.insert i en map))
    
cevents2IntMap :: [CEvent] -> IntMap [String]
cevents2IntMap cevs = snd $ foldr foldCEvents (0, IntMap.empty) cevs
  where
    foldCEvents :: CEvent -> (Int, IntMap [String]) -> (Int, IntMap [String])
    foldCEvents (CEvent en eargs) (i, map) = (i+2, IntMap.insert (i+1) eargs (IntMap.insert i [en] map))
     -- foldr (\v (k,m)-> (k+1, IntMap.insert k v m)) (i, map) (conEvt2List ce)

checkRRPat :: RRPattern RewRulPat t -> Log -> [(Log, [Int])] -> Int -> (Bool, RewRulPat)
checkRRPat pat log substrs n = 
  let wSet =  [(lhs, rhs)| (lhs, rhs) <- mkWSet pat log substrs
                         , (state $ last lhs) 
                           == 
                           (state $ last rhs)
                         ]
  in if (length wSet >= n)
     then (True,  getRule pat)
     else (False, getRule pat)
          
returnNegWit :: RRPattern RewRulPat t -> Log -> [(Log, [Int])] -> [(Log, Log)]
returnNegWit pat log substrs = 
  [(lhs, rhs)| (lhs, rhs) <- mkWSet pat log substrs
             , (state $ last lhs) 
               /= 
               (state $ last rhs)
             ]

-- Added filltering of completely equal witnesses. They should be counted as one 
-- witness. Because if we assume that corresponding real initial and final state 
-- are the same, it doesn't give us much more confidence to accept given pattern.
countPats :: Show t => [RRPattern RewRulPat t] -> Log -> [Witness]
countPats pats log = 
  let substrs = subseqsOfLog 2 log
      countPat pat = WitnessC 
        (getPattern pat) 
        (getRule pat)
        ( foldr (\(lhs, rhs) (pos, neg) -> 
                  if (state $ last lhs) == (state $ last rhs)
                  then (pos+1, neg)
                  else (pos, neg+1)
                ) (0,0) (nub $ mkWSet pat log substrs)
        )
  in map countPat pats
     

-- | if it works then try to drop the arguments to make this function more nicely looking
-- | to think about substituting this function into returnWitness
rewRulePatInf :: Show t => ([AEvent] -> [RRPattern RewRulPat t]) -> [AEvent] -> Log -> [Witness]
rewRulePatInf pat aevts log = countPats (pat aevts) log

returnWitnesses :: (Show t) => Filter -> ([AEvent] -> [RRPattern RewRulPat t]) ->  [AEvent] -> Log -> [Witness]
returnWitnesses flt = \pat aevts log -> filterWitnesses flt $ rewRulePatInf pat aevts log

-- | this function should be reworked to infer patterns from multiple logs
-- | for now it's assumed that it handles only singletone list 
-- | infer concrete algebraic rewrite rules
inferConAlgRRules :: Filter -> [AEvent] -> Log -> [Witness] 
inferConAlgRRules flt evs log =
  let skpRs          = returnWitnesses flt skip evs log
      skpEvs         = map (symEName . head . getLhsRewRulPat . getRewRulPat) skpRs
      nonSkpEs       = evs \\ skpEvs
      zrRs           = returnWitnesses flt zero   nonSkpEs log 
      idpRs          = returnWitnesses flt idemp  nonSkpEs log 
      ignRs          = returnWitnesses flt ignore nonSkpEs log
      comRs          = returnWitnesses flt com    nonSkpEs log
  in concat [skpRs, zrRs, idpRs, ignRs, comRs]
     