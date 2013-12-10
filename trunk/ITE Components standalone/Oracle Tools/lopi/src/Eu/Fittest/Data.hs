{-# LANGUAGE TypeFamilies, GADTs, ParallelListComp, RankNTypes, ExistentialQuantification #-}

module Eu.Fittest.Data where

import Data.List (intersectBy, deleteFirstsBy)
import Data.Tuple (swap)
import Data.Hashable

-- abstact event representation
type AEvent = String

-- concrete event represenation
type CEArgs = [String] -- use string representation for integers
data CEvent = CEvent { conEName :: AEvent 
                     , conEArgs :: CEArgs
                     }
              deriving (Show, Eq, Read)

-- symbolic event representation
type SEArgName = String
data SEArgs = SEArgs {symEvtArgs :: SEArgName}
           deriving (Show, Eq, Ord)
--type SEArgs = [SEArg]
data SEvent = SEvent { symEName :: AEvent
                     , symEArgs :: SEArgs
                     }
              deriving (Show, Ord)


data AlgRule = ASkip {getASkip :: AEvent}
             | AZero { getAZeroElem :: AEvent 
                     , getAZero     :: AEvent
                     }
             | ACom  AEvent AEvent
             | AOther
             deriving (Show, Eq, Ord)
                      
symRl2AlgRl :: RewRulPat -> AlgRule
symRl2AlgRl ([SEvent e (SEArgs _)] :~: []) = ASkip e
symRl2AlgRl ([SEvent d (SEArgs _), SEvent e (SEArgs _)] 
             :~: 
             [SEvent _ (SEArgs _)]
            ) = AZero d e
symRl2AlgRl ([SEvent e (SEArgs _), SEvent d (SEArgs _)] 
                 :~: 
             [SEvent _ (SEArgs _), SEvent _ (SEArgs _)]
            ) = ACom e d                   
symRl2AlgRl _ = AOther

instance Eq SEvent where
  (SEvent en1 _) == (SEvent en2 _) = en1 == en2

-- instance Ord SEvent where
--   compare ev1 ev2 | ev1 == ev2 = EQ
--                   | otherwise  = LT

type Var = String
data State = State [Var]
             deriving (Show, Eq, Read)

data LogEntry = LogEntry { state :: State 
                         , event :: CEvent
                         }
              deriving (Show, Read)
                       
instance Hashable LogEntry where
  hashWithSalt salt (LogEntry _ (CEvent e _)) = hashWithSalt salt e

instance Eq LogEntry where                       
  (==) (LogEntry _ (CEvent e1 _)) (LogEntry _ (CEvent e2 _)) = e1 == e2 
type Log = [LogEntry]

type Pat = [SEvent]
data RewRulPat = Pat :~: Pat
               deriving (Show, Ord)
                        
getSkipRules :: [RewRulPat] -> [RewRulPat]
getSkipRules rrs = [rr | rr <- rrs, isSkip rr] 

getZeroRules :: [RewRulPat] -> [RewRulPat]
getZeroRules rrs = [rr | rr <- rrs, isZero rr]

getComRules :: [RewRulPat] -> [RewRulPat]
getComRules rrs = [rr | rr <- rrs, isCom rr]

mkZrFrSkp :: [AEvent] -> [AEvent] -> [RewRulPat]
mkZrFrSkp evs zevs = [zeroRP e d | e <- evs, d <- zevs]

mkIdpFrSkp :: [AEvent] -> [RewRulPat]
mkIdpFrSkp zevs = [idempRP e | e <- zevs]

mkIgnFrSkp :: [AEvent] -> [RewRulPat]
mkIgnFrSkp zevs = [ignoreRP e | e <- zevs]

mkComFrSkp :: [AEvent] -> [AEvent] -> [RewRulPat]
mkComFrSkp evs zevs = [comRP e d | e <- zevs, d <- evs]

getLhsRewRulPat :: RewRulPat -> Pat
getLhsRewRulPat (lhs :~: _) = lhs

getRhsRewRulPat :: RewRulPat -> Pat
getRhsRewRulPat (_ :~: rhs) = rhs

instance Eq RewRulPat where
  (==) (l1 :~: r1) (l2 :~: r2) = (l1 == l2 && r1 == r2) || 
                                 (l1 == r2 && r1 == l2)

-- The encoding below as GADTs should be reconsidered in the future.
-- I have a fillng that with data famalies we could join all pattern types 
-- in one data type and apply rrpi algorithm to it. Now we have to handle each
-- type of rr-s individually. 
  
data Skip = Skip
          deriving Show
data Zero = Zero
          deriving Show
data Idemp = Idemp
           deriving Show
data Unknown = Unknown
             deriving Show
data Ignore = Ignore
            deriving Show
data Com = Com
          deriving Show

data RRPattern p t where
  SkipLike       :: p -> RRPattern p Skip
  ZeroLike       :: p -> RRPattern p Zero
  IdempLike      :: p -> RRPattern p Idemp 
  IgnoreLike     :: p -> RRPattern p Ignore
  ComLike        :: p -> RRPattern p Com
  UnknownPattern ::      RRPattern p Unknown
  
-- Function rrp2Pair and rrp2List might be useful for union of all rrp.
-- They give kind of common base for all rrp.
rrp2Pair :: RRPattern p t -> (p, t)
rrp2Pair (SkipLike p)   = (p, Skip)
rrp2Pair (ZeroLike p)   = (p, Zero)
rrp2Pair (IdempLike p)  = (p, Idemp)
rrp2Pair (IgnoreLike p) = (p, Ignore)
rrp2Pair (ComLike p)    = (p, Com)
rrp2Pair _ = error  "there is no such a pattern"

rrp2List :: [RRPattern p t] -> [(p, t)]
rrp2List rrps = map rrp2Pair rrps 

getPattern :: RRPattern p t -> t
getPattern (SkipLike   _)   = Skip
getPattern (ZeroLike   _)   = Zero
getPattern (IdempLike  _)   = Idemp
getPattern (IgnoreLike _)   = Ignore
getPattern (ComLike    _)   = Com
getPattern (UnknownPattern) = error "Type of pattern is not deffined"

getRule :: RRPattern p t -> p
getRule (SkipLike   p) = p
getRule (ZeroLike   p) = p
getRule (IdempLike  p) = p
getRule (IgnoreLike p) = p
getRule (ComLike    p) = p
getRule UnknownPattern = error "There is no rule defined"

skipRP :: AEvent -> RewRulPat
skipRP e = [SEvent e (SEArgs "p")] :~: []

isSkip :: RewRulPat -> Bool
isSkip ([SEvent _ (SEArgs _)] :~: []) = True 
isSkip _                              = False 

skipLike :: AEvent -> RRPattern RewRulPat Skip
skipLike e = SkipLike $ skipRP e

skip :: [AEvent] -> [RRPattern RewRulPat Skip]
skip es = [ skipLike e| e <- es ]


zeroRP :: AEvent -> AEvent -> RewRulPat
zeroRP e d =  [SEvent d (SEArgs "p"), SEvent e (SEArgs "q")] 
              :~: 
              [SEvent e (SEArgs "q")]

-- isZero rule also returns true for idempotent patterns
isZero :: RewRulPat -> Bool
isZero ([SEvent _ (SEArgs _), SEvent e (SEArgs q)] 
        :~: 
        [SEvent e1 (SEArgs q1)]
       ) = e == e1 && q == q1 
isZero _ = False           

zeroLike :: AEvent -> AEvent -> RRPattern RewRulPat Zero
zeroLike e d = ZeroLike $ zeroRP e d

zero :: [AEvent] -> [RRPattern RewRulPat Zero]
zero es = [zeroLike e d | e <- es, d <- es, e /= d]

idempRP :: AEvent -> RewRulPat
idempRP e = [SEvent e (SEArgs "p"), SEvent e (SEArgs "q")] 
            :~: 
            [SEvent e (SEArgs "q")]
              
idempLike :: AEvent -> RRPattern RewRulPat Idemp
idempLike e = IdempLike $ idempRP e
              
idemp :: [AEvent] -> [RRPattern RewRulPat Idemp]
idemp  es = [idempLike e | e <- es]

ignoreRP :: AEvent -> RewRulPat
ignoreRP e = [SEvent e (SEArgs "p")] 
             :~: 
             [SEvent e (SEArgs "q")]

ignoreLike :: AEvent -> RRPattern RewRulPat Ignore 
ignoreLike e = IgnoreLike $ ignoreRP e

ignore :: [AEvent] -> [RRPattern RewRulPat Ignore]
ignore es = [ignoreLike e| e <- es]

comRP :: AEvent -> AEvent -> RewRulPat
comRP e d = [SEvent e (SEArgs "p"), SEvent d (SEArgs "q")] 
            :~: 
            [SEvent d (SEArgs "q"), SEvent e (SEArgs "p")]

isCom :: RewRulPat -> Bool
isCom ([SEvent e (SEArgs p), SEvent d (SEArgs q)] 
       :~: 
       [SEvent d1 (SEArgs q1), SEvent e1 (SEArgs p1)]
      ) = e == e1 && p == p1 && d == d1 && q == q1  

comLike :: AEvent -> AEvent -> RRPattern RewRulPat Com
comLike e d = ComLike $ comRP e d
              

com :: [AEvent] -> [RRPattern RewRulPat Com]
com es = let allComPairs = [(e, d) |e <- es, d <- es, e /= d]
             fillterComPairs = foldr (\x y -> 
                                       if (swap x) `elem` y 
                                       then y 
                                       else (x:y)
                                     ) 
                               [] 
                               allComPairs
         in [comLike e d | (e, d) <- fillterComPairs]


-- it is how I would like to write join rrp of all types
-- allRRPats :: [AEvent] -> RRPattern RewRulPat t
-- allRRPats es = concat $ map (flip ($) es) $ rrp2List [skip, zero]


-- | Possibly change Witness to a datatype
data Witness where
  WitnessC :: Show a =>  a -> RewRulPat -> (Int, Int) -> Witness
  
instance Show Witness where
  show (WitnessC t rr wc) = show rr ++ show wc ++ show t

instance Eq Witness where
   (WitnessC _ rr1 _) ==  (WitnessC _ rr2 _) = rr1 == rr2

type Witnesses = [Witness]

getRewRulPat :: Witness -> RewRulPat
getRewRulPat (WitnessC _ r _) = r

getWitnessCount :: Witness -> (Int, Int)
getWitnessCount (WitnessC _ _ c) = c

getWitnessType :: Witness -> String
getWitnessType (WitnessC t _ _) = show t
                                   
type Template = [[RewRulPat]] 

data Result = Result { getMatchWit   :: [RewRulPat]
                     , getFPMatchWit :: [RewRulPat]
                     , getFNMatchWit :: [RewRulPat]
                     }

sumRes :: Result -> (Int, Int, Int)
sumRes (Result mat fp fn) = (length mat, length fp, length fn) 

matchWthTempl :: [RewRulPat] -> [RewRulPat] -> Result 
matchWthTempl twt wt = 
  let pred    = \x y -> x == y 
      match   = intersectBy pred twt wt 
      matchFP = deleteFirstsBy pred wt twt
      matchFN = deleteFirstsBy pred twt wt
  in  (Result match matchFP matchFN)

appendWS :: Witnesses -> Witnesses -> Witnesses
appendWS ws1 ws2 = foldr (\x xs -> cons x xs) ws1 ws2 where
  cons w []      = [w]
  cons w1@(WitnessC t1 rr1 (p1,n1)) (w2@(WitnessC _ rr2 (p2,n2)):ws)
    | rr1 == rr2 = (WitnessC t1 rr1 (p1+p2,n1+n2)):ws
    | otherwise  = w2:(cons w1 ws)

concatWS :: [Witnesses] -> Witnesses
concatWS = foldr appendWS []
  