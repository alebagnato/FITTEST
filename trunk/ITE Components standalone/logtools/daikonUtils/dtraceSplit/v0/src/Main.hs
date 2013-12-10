{-

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- |

   Small tool to split daikon .dtrace files to separate declaration
   parts and the rest.

-}
module Main

where

import Data.List
import System.Environment
import System.IO

-- The main splitter function.
-- The split is identified by the first ppt record in the Daikon .dtrace.
-- A ppt record can be identified by its 2nd line, which always start with
-- the string this_invocation_nonce.
--
splitFile :: FilePath -> IO()
splitFile f = do {
     putStr ("** Spliting " ++ f ++ " ...") ;
     input <- readFile  f ;
     (decl,records) <- return . splitInput $ input ;
     writeFile declf decl ;
     writeFile recordf records ;
     putStrLn "done." ;
  }
  where
  basename = reverse . tail . dropWhile (/= '.') . reverse $ f
  declf    = basename ++ ".decls"
  recordf  = basename ++ ".recs.dtrace"  
   
splitInput input = (decl,records)
   where
   (s1,s2) = split_ (lines input)
   decl    = unlines s1
   records = unlines s2
   
split_  []  = ([],[])
split_  [l] = ([],[l])
split_  z@(l1:l2:rest)  =  if ("this_invoc" `isPrefixOf` (dropWhile isWhite l2)) 
	                         then ([],z)
	                         else  let
                                   (s1,s2) = split_ (l2:rest)
				                   in
                                   (l1:s1,s2)
                              
isWhite c = c == ' ' ||  c == '\t' 

                                   
main :: IO()
main = do { args <- getArgs ; main_ args }

mainf args = main_ (words args)

main_ args = do {
   case args of
      ("--help" : _)  ->  putStrLn "dtrsplit file [file]+"
      files           ->  sequence_ [ splitFile f | f <- files ]
   }
   