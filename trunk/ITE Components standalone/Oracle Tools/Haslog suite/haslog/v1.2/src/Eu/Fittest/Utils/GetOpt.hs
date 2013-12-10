{- 

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- |

   This module extends System.Console.GetOpt with hierarchical options.
   
-}

module Eu.Fittest.Utils.GetOpt (
    OptionTree(..),
    getOptionsFromOptionTrees,
    opt1,
    optg,
    optArg_,
    usageTInfo
  )
where

import System.Console.GetOpt
  
data OptionTree a = OptionGroup (OptDescr a)  [OptionTree a]

getRootOpt (OptionGroup o _) = o

getOptionsFromOptionTrees = flatten_

flatten_ [] = []
flatten_ (OptionGroup o sub : rest) = o : (flatten_ sub ++ flatten_ rest)  

opt1 s l a h      = OptionGroup (Option s l a h) []
optg s l a h subs = OptionGroup (Option s l a h) subs

optArg_ f default_ name = OptArg (\x-> case x of
                                      Just i -> f i
                                      _      -> f default_ )  
                                name                                       
   
   
usageTInfo :: String -> [OptionTree a] -> String
usageTInfo header options = header ++ "\n" ++ usage options
   where   
   usage options =  unlines . concat $ r
      where     
      r = zipWith glue rootOptions subs
      --glue x [] = ["", x]
      --glue x y  = ["", x, "", indent 3 "** Suboptions:", y]
      glue x [] = [x]
      glue x y  = [x, indent 3 "|* Suboptions:", y]
      
      rootOptions = map ("|" ++) . removeEmpty . lines . usageInfo "" . map getRootOpt $ options
      subs = [ reformat (usage sub) | OptionGroup o sub <- options ]
      reformat d = unlines_ [ indent 3 s | s <- lines d ]
      indent k s = replicate k ' ' ++ s   
      unlines_ [] = []
      unlines_ s = init (unlines s)      
      removeEmpty s = filter (not . null) s

      
-- example
ex1 = [t,t]
   where
   t = optg "h" ["help"] (ReqArg (const 0) "arg") "desc1.."
            [u ,
             opt1 "h" ["help"] (ReqArg (const 0) "arg") "desc1.." ]
   u = optg "x" ["xxx"] (ReqArg (const 0) "arg") "more desc..."
           [opt1 "z" ["zelp"] (ReqArg (const 0) "arg") "desc1..",
            opt1 "z" ["zelp"] (ReqArg (const 0) "arg") "desc1.." ]
       
             



    
  