{- 

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 

   This module provides ...
     
-}

module Eu.Fittest.Logging.Analysis.Base 

where

import Data.List
import Data.Maybe

-- | Representing a way to treat a log entry as if it is a tuple
--   of event, its parameter, and the state after the event. Note
--   that multiple parameters can be encoded as e.g. a list.
--
type EPS event param state = (event,param,state)

-- some utility functions

event_ (e,_,_) = e
param_ (_,p,_) = p
state_ (_,_,s) = s
eventTr_ = map event_
paramTr_ = map param_
stateTr_ = map state_

-- Function to turn string to simulated sequence of events. We pretend
-- a string s to describe (event1,param,state1):(event2,param,state2) ...
-- Useful for testing.
--
mkEPS s = concat . map f . chunk 3 $ s
    where
    f [e,p,x] = [(e,p,x)]
    f _       = []   
   
    
chunk k s = worker s
   where
   worker [] = []
   worker s  = prefix : worker rest
      where
      (prefix,rest) = splitAt k s     
    
forall_ s p = all p s
exists_ s p = any p s
imp_ p q    = not p || q   
zyp2 s  = zip s (tail s)
zyp3 s  = zipWith (\x (y,z)->(x,y,z)) s (tail (zyp2 s))


-- =====================================================
-- Functions to infer basic informations from a log.
-- =====================================================

events_ :: Eq e => [EPS  e p s] -> [e]
events_  =  nub . eventTr_

params_ :: Eq p => [EPS e p s] -> [p]
params_  =  nub . paramTr_

states_ :: Eq s => [EPS e p s] -> [s]
states_  =  nub . stateTr_ 

eventsParams :: (Eq e, Eq p) => [EPS e p s] -> [(e,[p])]
eventsParams s = [ (e,getParams e) | e <- events_ s ]
   where
   getParams e = params_ . filter (\x-> event_ x == e) $ s


-- | Check if a given population size represents enough sampling.
--   Sampling theory: 95% confidence, with 10% error bound gives
--   sampling size n =100
-- 
sufficientPopSize n = n>1  -- 100

-- | Infer side-effect free events from the given log. 
--   An event e is side-effect free if:
--
--     (1) for all s0, s0->e(p)->t : t=s0
--     (2) for all s0,p,q,and observation tau:
--             s0->e(p)->tau->t1
--             s0->e(q)->tau->t2
--         then t1=t2
--
--   The implementation below is too simplistic.
--
readEvents :: (Eq e, Eq s) => [EPS e p s] -> [e]
readEvents s = [ e | e <- events_ s, isReadEvent e ]
   where
   isReadEvent e = sufficientPopSize (length states)
                   &&
                   forall_ s2 (\(x,y)-> state_ x == state_ y)
      where
      s2 = filter (\(x,y)-> event_ y == e) . zyp2 $ s
      states = states_ . map fst $ s2

-- | Infer events which have side effect, but are memory-less (does not
--   depend on the previous state)
--   An event e is a memory-less assignment if:
--
--    (1) it has side effect: there is an s0, sucht that s0->e->t and s0<>t
--    (2) for all s,t,p : s0->e(p)->t0 and s1->e(p)->t1 ==> t0=t1
--
-- The naming below is not good, and definition is also not good.
--
assignmentLikeEvents :: (Eq e, Eq p, Eq s) => [EPS e p s] -> [e]
assignmentLikeEvents s = [ e | (e,params) <- eventsParams s, isAsg e params ]
   where
   isAsg e params =  sufficientPopSize (length params) 
                     &&
                     forall_  params
                              (\p-> allEqual . states_ . filter (\x-> param_ x == p) $ s2)
       where
       s2 = filter (\x-> event_ x == e) s
       
allEqual [] = True
allEqual [x] = True
allEqual (x:s@(y:_)) = x==y && allEqual s

halfMatrix [] = []
halfMatrix (x:s) = map (\y-> (x,y)) s  ++  halfMatrix s

-- | Infer pairs of independent events. Two event d and e are
--   independent if for all state s0:
--
--         s0->d(x);d(y)  and  s0->d(y);d(x)
--
--   result in the same state.
--
indepEvents :: (Eq e, Eq p, Eq s) => [EPS e p s] -> [(e,e)]
indepEvents s = [ (d,e) | (d,e)<-eventsPairs , indep d e ]
    where
    eventsPairs = halfMatrix . events_ $ s
    sss    = zyp3 s
    indep d e = sampleSize >= 1
                &&
                forall_ 
                  sample 
                  (\((_,_,e1),matches) -> forall_ 
                                             matches 
                                             (\(_,_,d2)-> state_ e1 == state_ d2)
                  )                                    
       where
       s_de = filter (\(x,y,z)-> event_ y == d && event_ z == e) sss
       s_ed = filter (\(x,y,z)-> event_ y == e && event_ z == d) sss
       sample = map (\u-> (u, filter (match u) s_ed)) s_de
       match (x1,d1,e1) (x2,e2,d2) = state_ x1 == state_ x2
                                     &&
                                     param_ d1 == param_ d2
                                     &&
                                     param_ e1 == param_ e2
       sampleSize = sum . map (\(u,matches)-> length matches) $ sample
       
       
-- =====================================================
-- tests
-- 
-- Should be tested with sufficientPopSize n = n>1 
-- =====================================================

testEv = zip [1..]
         [ (events_ . mkEPS $ "") == "",
           (events_ . mkEPS $ "ex1dy1ex2dz2fx2") == "edf"
         ]
         
testRE = zip [1..]
         [ (readEvents . mkEPS $ "") == "",
           (readEvents . mkEPS $ "abc") == "",
           (readEvents . mkEPS $ "ex1dy1ex2dz2fx2") == "d",
           (readEvents . mkEPS $ "ex1dy1dz1") == ""
         ]
         
testALE = zip [1..]
          [ (assignmentLikeEvents . mkEPS $ "") == "",
            (assignmentLikeEvents . mkEPS $ "abc") == "",
            (assignmentLikeEvents . mkEPS $ "abcabc") == "",
            (assignmentLikeEvents . mkEPS $ "apxaqy") == "a",
            (assignmentLikeEvents . mkEPS $ "apxaqyapy") == ""    
          ]
          
testIndep = zip [1..]
          [ (indepEvents . mkEPS $ "") == [],
            (indepEvents . mkEPS $ "abc") == [],
            (indepEvents . mkEPS $ "apxapx") == [],
            (indepEvents . mkEPS $ "apxbqy") == [],
            (indepEvents . mkEPS $ "a-sdp-eqtaoxa*seq-dpt") == [('d','e')]       
          ]
 
-- =====================================================
-- Functions to reduce a given segment of a log.
-- ===================================================== 
 
type Rule_ a = a -> Maybe a

-- | to remove a set of events.
remove_ :: Eq e => [e] -> Rule_ [EPS e p s]
remove_ events s = Just (filter (\x-> event_ x `notElem` events) s)

-- | If e is an assignment event, reduce e(p);e(q) --> e(q)
--
asgRule_ :: (Eq e, Eq p, Eq s) => [e] -> Rule_ [EPS e p s]
asgRule_  asgEvents s = if Nothing `elem` reduced_ 
                        then Just (reduced ++ [last s])
                        else Nothing
   where
   reduced  = map fromJust . filter isJust $ reduced_
   reduced_ = map reduce . zyp2 $ s
   reduce (x,y) = if event_ x `elem` asgEvents
                     &&
                     event_ x == event_ y
                     then Nothing
                     else Just x
         
removeIndep_ :: Eq e => [(e,e)] -> Rule_ [EPS e p s]          
removeIndep_ indepPairs s = Just . worker . reverse $ s
   where
   worker s = worker_ [head s] (tail s)
   worker_  prefix [] = prefix
   worker_  prefix (x:t) = if forall_ prefix (\y-> (event_ x, event_ y) `elem` indepPairs
                                                  ||   
                                                 (event_ y, event_ x) `elem` indepPairs)
                         then worker_ prefix t
                         else worker_ (x:prefix) t
        
repeat_ ::  Rule_ [EPS e p s] -> Rule_ [EPS e p s]         
repeat_ rule s = case rule s of
                   Nothing -> Just s
                   Just t  -> repeat_ rule t
                     
simplify_ :: (Eq e, Eq p, Eq s) => [e] -> [e] -> [(e,e)] -> Rule_ [EPS e p s]                 
simplify_ readEvents asgEvents indepPairs s 
   = 
   do { t <- remove_ readEvents s ;
        u <- repeat_  (asgRule_  asgEvents) t ;
        v <- removeIndep_ indepPairs u ;
        return v 
      }                     
      
-- =====================================================
-- tests 
-- =====================================================   

testSIMP = zip [1..]
           [ (fromJust . simplify_ "ab" [] [] . mkEPS $ "cx1ax1dx1by2cx1") 
              == 
              mkEPS "cx1dx1cx1",
              
              (fromJust . simplify_ [] "ab" [] . mkEPS $ "cx1ax1ay2bx1") 
              == 
              mkEPS "cx1ay2bx1",
              
              (fromJust . simplify_ [] [] [('a','b'),('a','c')] . mkEPS $ "cx1ax1cy2bx1") 
              == 
              mkEPS "cx1cy2bx1"
           ]
           