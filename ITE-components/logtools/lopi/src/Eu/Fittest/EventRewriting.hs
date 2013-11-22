module Eu.Fittest.EventRewriting (runRewrite, runSkipRewrite)  where

import Eu.Fittest.Data
import GHC.Exts (groupWith)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (find)
import Debug.Trace

runSkipRewrite :: [RewRulPat] -> [CEvent] -> [CEvent]
runSkipRewrite rrs log = normalize rrs log

runRewrite ::  [RewRulPat] -> [CEvent] -> [CEvent]
runRewrite rrs log =  
  let algrs  = map symRl2AlgRl rrs
      zrrs   = filter (\algr -> case algr of 
                          AZero _ _ -> True
                          _         -> False
                      ) algrs
      cmrs   = filter (\algr -> case algr of 
                          ACom _ _ -> True
                          _        -> False
                      ) algrs
      zrrs'  = zrrs ++ zeroOverlap zrrs 
      izrrs  = inferIZero zrrs' cmrs
      izrrs' = izrrs ++ zeroOverlap izrrs
      zrCls  = map (\cl -> (getAZero $ head cl, cl))  
               $ groupWith (\(AZero x y) -> y) zrrs'
      izrCls = map (\cl -> (getAZero $ head cl, cl))  
              $ groupWith (\(AZero x y) -> y) izrrs'
      nlog   = normalize rrs log
  in 
   fixP ( reverse 
          . fixZeroRewrite izrCls cmrs [] 
          . reverse 
          . fixZeroRewrite zrCls cmrs []
        ) nlog
     
   
-- only one transformation has been emplemented yet. namely, skip removing     
-- to do: inferring additional zero rues (zero + skip => zero')
normalize :: [RewRulPat] -> [CEvent] -> [CEvent]  
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

applySkip :: [AlgRule] -> [CEvent] -> [CEvent]
applySkip rs l = 
  let sevs = map getASkip rs
  in foldr (\hd tl -> 
             let (CEvent e arg) = hd
             in if e `elem` sevs
                then tl
                else hd:tl
           ) [] l

type Pos   = ([CEvent], CEvent, [CEvent])

type ZClass = (AEvent,[AlgRule])

findFstZeroApp :: [(AEvent,[AlgRule])] -> [CEvent] -> Maybe (ZClass, Pos)
findFstZeroApp _    []  = Nothing
findFstZeroApp zcls (l:ls) = 
  let (CEvent e arg) = l
      mcl = find (\(x, xcl) -> x == e) zcls
      findNext = findFstZeroApp zcls ls
  in case mcl of
    Just cl -> Just (cl, ([], l, ls))
    Nothing -> case findNext of
      Just (cl, (u,w,v)) -> Just (cl, (l:u,w,v))
      Nothing            -> Nothing  

applyZeroFix :: [AlgRule] -> [CEvent] -> [CEvent]
applyZeroFix zrs ls = fixP (applyZero zrs) ls
  where
    applyZero :: [AlgRule] -> [CEvent] -> [CEvent]
    applyZero _ [] = [] 
    applyZero rs log@(lg:lgs) = 
      let (CEvent e arg) = lg
          zrels = map getAZeroElem rs
      in  if (e `elem` zrels)         
          then lgs
          else log
          
fixP :: Eq a => (a -> a) -> a -> a
fixP f = until (\x -> f x == x) f

applyZeroCom :: [AlgRule] -> [AlgRule] -> [CEvent] -> [CEvent]
applyZeroCom zrrs cmrs log = 
  let  
    findZeroCom :: [AlgRule] -> ([CEvent], Maybe CEvent, [CEvent]) -> ([CEvent], Maybe CEvent, [CEvent]) 
    findZeroCom zrs (v, x, []) = (v, x, [])
    findZeroCom zrs (u, x, v)  = 
      let zrels = map getAZeroElem zrs
          hd = head v
          (CEvent e arg) = hd
      in  if (e `elem` zrels) && (comWithAll cmrs e u)
          then (u, Just hd, tail v)
          else findZeroCom zrs (u ++ [hd], x, tail v)
    (pre, match, post) = findZeroCom zrrs ([], Nothing, log)
    comWithAll :: [AlgRule] -> AEvent -> [CEvent] -> Bool
    comWithAll crs e l = 
      let  
        ev = map conEName l
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
  
zeroRewrite :: [AlgRule] -> [AlgRule] -> [CEvent] -> [CEvent]
zeroRewrite zrrs cmrs = 
  fixP (applyZeroCom zrrs cmrs . applyZeroFix zrrs)
  
fixZeroRewrite :: [ZClass] -> [AlgRule] -> [CEvent] -> [CEvent] -> [CEvent]
fixZeroRewrite zcls cmrs acc evs = 
  case findFstZeroApp zcls evs of
    Just ((cn,zrs),(pref,le,suff)) -> 
      let racc' = zeroRewrite zrs cmrs (reverse (acc ++ pref))
          acc'  = reverse racc' ++ [le]
      in  fixZeroRewrite zcls cmrs acc' suff      
    Nothing -> acc ++ evs  
    
zeroOverlap :: [AlgRule] -> [AlgRule]
zeroOverlap zrs = 
  let zrs' = Set.fromList zrs
      checkZOverlap :: AlgRule -> AlgRule -> Set AlgRule
      checkZOverlap (AZero e z) (AZero e1 z1) | z  == e1  = Set.singleton (AZero e z1)
                                              | z1 == e   = Set.singleton (AZero e1 z)
                                              | otherwise = Set.empty
      zeroOverlapOne :: AlgRule -> Set AlgRule -> Set AlgRule
      zeroOverlapOne r rs = 
        r `Set.insert` Set.foldr (\r1 s -> Set.insert r1 (checkZOverlap r r1) 
                                           `Set.union` s
                                 ) Set.empty  rs
  in  Set.toList $ Set.foldr (\hr tr -> zeroOverlapOne hr tr) zrs' zrs'
      

inferIZero :: [AlgRule] -> [AlgRule] -> [AlgRule]
inferIZero zrs crs = foldr (\hr tr -> case checkIZero hr crs of
                               Just (AZero e z) -> (AZero e z):tr
                               Nothing          -> tr
                           )  [] zrs
  where
    checkIZero (AZero e z) comrs = if any (\(ACom c1 c2) -> 
                                            (e == c1 && z == c2)
                                            ||
                                            (e == c2 && z == c1)
                                          ) comrs
                                   then Just (AZero e z)
                                   else Nothing