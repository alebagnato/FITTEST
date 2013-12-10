module Eu.Fittest.Rewriting  where

import Eu.Fittest.Data
import GHC.Exts (groupWith)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (find)
import Debug.Trace

runRewrite :: [RewRulPat] -> Log -> Log
runRewrite rrs log =  
  let algrs = map symRl2AlgRl rrs
      zrrs  = filter (\algr -> case algr of 
                         AZero _ _ -> True
                         _         -> False
                     ) algrs
      cmrs  = filter (\algr -> case algr of 
                         ACom _ _ -> True
                         _        -> False
                     ) algrs
      zrCls = map (\cl -> (getAZero $ head cl, cl))  
              $ groupWith (\(AZero x y) -> y) zrrs
      nlog = normalize rrs log
      findZeroRec h t = case findFstZeroApp zrCls t of
        Just ((cn,zrs),(pref,le,suff)) -> findZeroRec (reverse (zeroRewrite zrs cmrs (reverse pref)) ++ [le]) suff
        Nothing -> h ++ t
  in findZeroRec [] nlog
  
   
-- only one transformation has been emplemented yet. namely, skip removing     
-- to do: inferring additional zero rues (zero + skip => zero')
normalize :: [RewRulPat] -> Log -> Log  
normalize rrs log = 
  let algrs = map symRl2AlgRl rrs
      skrs  = filter (\algr -> case algr of 
                         ASkip _ -> True
                         _       -> False
                     ) algrs
      zrrs  = filter (\algr -> case algr of 
                         AZero _ _ -> True
                         _         -> False
                     ) algrs
      cmrs  = filter (\algr -> case algr of 
                         ACom _ _ -> True
                         _        -> False
                     ) algrs
      isZrSk :: AlgRule -> Set AlgRule -> Bool        
      isZrSk (AZero x y) srs = Set.foldr (\(ASkip z) tl -> 
                                           if z == y 
                                           then True 
                                           else False || tl  
                                         ) False srs 
      skFrZr zrs srs = Set.foldr (\zr tl -> 
                                   if isZrSk zr srs
                                   then Set.insert (ASkip $ getAZeroElem zr) tl
                                   else tl 
                                 ) Set.empty zrs       
      skrs' = Set.fromList skrs `Set.union` 
              skFrZr (Set.fromList zrrs) (Set.fromList skrs)
  in applySkip (Set.toList skrs') log

applySkip :: [AlgRule] -> Log -> Log
applySkip rs l = 
  let sevs = map getASkip rs
  in foldr (\hd tl -> 
             let (LogEntry s (CEvent e arg)) = hd
             in if e `elem` sevs
                then tl
                else hd:tl
           ) [] l

type Pos   = (Log, LogEntry, Log)

findFstZeroApp :: [(AEvent,[AlgRule])] -> Log -> Maybe ((AEvent,[AlgRule]), Pos)
findFstZeroApp _    []  = Nothing
findFstZeroApp zcls (l:ls) = 
  trace "findFstZeroApp" $
  let (LogEntry s (CEvent e arg)) = l
      mcl = find (\(x, xcl) -> x == e) zcls
      findNext = findFstZeroApp zcls ls
  in case mcl of
    Just cl -> Just (cl, ([], l, ls))
    Nothing -> case findNext of
      Just (cl, (u,w,v)) -> Just (cl, (l:u,w,v))
      Nothing            -> Nothing  

applyZeroFix :: [AlgRule] -> Log -> Log
applyZeroFix zrs ls = trace "applyZeroFix" $ fixP (applyZero zrs) ls
  where
    applyZero :: [AlgRule] -> Log -> Log
    applyZero _ [] = [] 
    applyZero rs log@(lg:lgs) = 
      let (LogEntry s (CEvent e arg)) = lg
          zrels = map getAZeroElem rs
      in  if (e `elem` zrels)         
          then lgs
          else log
          
fixP :: Eq a => (a -> a) -> a -> a
fixP f = until (\x -> f x == x) f

applyZeroCom :: [AlgRule] -> [AlgRule] -> Log -> Log
applyZeroCom zrrs cmrs log = 
  trace "applyZeroCom" $
  let  
    findZeroCom :: [AlgRule] -> (Log, Maybe LogEntry, Log) -> (Log, Maybe LogEntry, Log) 
    findZeroCom zrs (v, x, []) = (v, x, [])
    findZeroCom zrs (u, x, v)  = 
      let zrels = map getAZeroElem zrs
          hd = head v
          (LogEntry s (CEvent e arg)) = hd
      in  if (e `elem` zrels) && trace (show $ comWithAll cmrs e u) (comWithAll cmrs e u)
          then (u, Just hd, tail v)
          else findZeroCom zrs (u ++ [hd], x, tail v)
    (pre, match, post) = findZeroCom zrrs ([], Nothing, log)
    comWithAll :: [AlgRule] -> AEvent -> Log -> Bool
    comWithAll crs e l = let ev = map (conEName . event) l
                             checkCom t evs = all (\evn -> any (\(ACom e1 e2) -> 
                                                                 (e1 == evn && e2 == t) 
                                                                 || 
                                                                 (e1 == t && e2 == evn)
                                                               ) crs
                                                  ) evs
                         in checkCom e ev
  in  case match  of
    Nothing -> pre
    Just m  -> pre ++ post
  
zeroRewrite :: [AlgRule] -> [AlgRule] -> Log -> Log
zeroRewrite zrrs cmrs = 
  fixP (applyZeroCom zrrs cmrs . applyZeroFix zrrs)
 
           