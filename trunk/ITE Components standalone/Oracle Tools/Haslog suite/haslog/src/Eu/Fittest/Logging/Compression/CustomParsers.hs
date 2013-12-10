{-

Author: Wishnu Prasetya

Copyright 2012 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- |

  This module provides a custom made parser combinators. The main feature is that it is
  faster and consumes less memory. We have tried the ReadP parser which comes with the
  standard Haskell Platform set of library, and also UU parsing lib. Both do not scale
  up enough to process large log files (> 30MB). It might be possible to change some
  low level details of those standard parsing libs to boost up the performance; we may
  experiment with that in the future. For now, we go with this custom library.

-}

{-# LANGUAGE BangPatterns #-}

module Eu.Fittest.Logging.Compression.CustomParsers

where

import Data.List
import Data.Char
import Data.Int

type Parser a = String -> (Maybe a,String)

spaces__ = " \n\t\r"

isSpace_ !c = c `elem` spaces__

skipSpaces :: Parser ()
skipSpaces s = (Just (), dropWhile isSpace_ s)

pToken :: String -> Parser ()
pToken !t !s = if t `isPrefixOf` s 
                then (Just (), drop (length t) s)
                else (Nothing, s)
                
pChar :: Char -> Parser ()                
pChar !c [] = (Nothing,[])
pChar !c !s@(x:t) | c==x      = (Just (), t)
                 | otherwise  = (Nothing,s)                
  
-- without sign  ; will first read the int as Int64, and then convert it to Integer;
-- seems to be considerably more efficient to do it like that
pInteger_ :: Parser Integer
pInteger_ ![] = (Nothing,[])
pInteger_ !s0@(x:s) 
   | isDigit_ x = let
                  (Just i,rest) = pInteger__ (char2Int64 x) s 
                  in
                  (Just (toInteger i), rest)
   | otherwise  = (Nothing, s0)

   
zero__ = ord '0'
nine__ = ord '9'
isDigit_ !c = let
              c_ = ord c  
              in
              c_ >= zero__ && c_ <= nine__    
           
char2Int64 :: Char -> Int64
char2Int64 !x = fromIntegral (ord x - zero__)

pInteger__ :: Int64 -> Parser Int64
pInteger__ !i ![] = (Just i, [])
pInteger__ !i !s0@(x:s) | isDigit_ x = pInteger__ (char2Int64 x + 10*i) s
                        | otherwise  = (Just i,  s0)  
                        
-- parsing integer turns out to consume quite a lot of CPU time and memory; so
-- i'm trying to optimize it with low level coding as above   
pInteger :: Parser Integer
pInteger = pInteger_
      <<|> (pChar '+' *> pInteger_)
      <<|> ((\_ i -> -i) <$> pChar '-' <*> pInteger_)
  
pAnyUntilToken :: String -> Parser String  
pAnyUntilToken !t !s = worker [] s
   where
   worker !z ![]       = (Nothing,[])
   worker !z !s@(x:s') =
    case pToken t s of
      !(Nothing,_)  -> worker (x:z) s'
      !(Just _,s'') -> (Just (reverse z), s'')

pAnyUntilChar :: Char -> Parser String  
pAnyUntilChar !c !s = worker [] s
   where
   worker !z ![]       = (Nothing,[])
   worker !z !(x:s') | c==x      = (Just (reverse z), s')
                     | otherwise =  worker (x:z) s'
      
infixl 4  <$>, <*>, <*, *>
infixr 3  <<|>
   
(p1 <*> p2) !s  = 
    case p1 s of
      !(Nothing, _)  -> (Nothing, s)
      !(Just f,  s2) -> 
         case p2 s2 of
            !(Nothing,s3) -> (Nothing, s3)
            !(Just x, s3) -> x `seq` (Just (f x), s3)     

            
p1 <* p2 = (\x _ -> x) <$> p1 <*> p2
p1 *> p2 = (\_ y -> y) <$> p1 <*> p2
            
(p1 <<|> p2) !s =
    case p1 s of
       !r@(Just _,_) -> r
       _             ->  p2 s


(f <$> p) !s = 
    case p s of
       !(Just x, s2) -> x `seq` (Just (f x), s2)
       _             -> (Nothing, s)
          
 
pSequence p !s = 
   case p s of
      !(Nothing,_)  -> (Just [], s)
      !(Just x, s2) -> let
                       !(Just z,s3) = pSequence p s2
                       in
                       x `seq` z `seq` (Just (x:z), s3)
                       
            
 