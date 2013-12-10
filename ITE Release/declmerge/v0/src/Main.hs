{-

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- |

   Small tool to merge mutiple Daikon decl files.

-}
module Main

where

import Data.List
import Data.Maybe
import System.Environment
import System.IO


type Decl   = [String]
type Header = [String]
type Lines  = [String]
type Parser a = Lines -> (a, Lines)

-- split the content of a decl-file l to its header and ppt-decls
splitInput :: String -> (Header,[Decl])
splitInput s = (header,decls)
  where
  z = lines s
  (header,rest) = pHeader z
  (decls,_)     = pDecls  rest
   
pHeader :: Parser Header
pHeader z = (header_, rest)
   where
   (header,rest) = break isPpt z
   header_  =  reverse . dropWhile isWhiteLine . reverse $ header

isPpt s = "ppt" `isPrefixOf` (dropWhile isWhite s)

pDecl :: Parser Decl
pDecl z = break isWhiteLine z
                 
pDecls :: Parser [Decl]
pDecls [] = ([],[])
pDecls z@(s:rest) = if isWhiteLine s then pDecls rest
                    else (d:ds, rest3)
        where
       (d,rest2)  =  pDecl z
       (ds,rest3) =  pDecls rest2


isWhite c     = c == ' ' ||  c == '\t' 
isWhiteLine s = all isWhite s

getPPTname :: Decl -> String
getPPTname decl = dropWhile isWhite . drop 3 $ line1
   where
    line1 = head decl   
 
newLine = "" 

-- merge the contents of a number of decl-files into one decl-content
mergeInputs :: [String] -> String
mergeInputs inputs = unlines . concat . intersperse [newLine] $ (header:mergedDecls)
   where
   (header,mergedDecls) = mergeInputs_ inputs
   
mergeInputs_ :: [String] -> (Header,[Decl])
mergeInputs_ inputs = (head headers, decls)
   where
   inputs_ = map splitInput inputs
   headers    = map fst inputs_
   declgroups = map snd inputs_
   alldecl    = concat declgroups
   pptNames   = nub . map getPPTname  $ alldecl

   decls = [ findDecl n | n <- pptNames ]
   
   findDecl name = fromJust . find (\d-> getPPTname d == name) $ alldecl
   
mergeFiles :: [FilePath] -> IO()
mergeFiles files = do {
     putStrLn ("** merging " ++ fileNames ++ " ...") ;
     inputs <- sequence [ readFile f | f <- files ] ;
     result <- return . mergeInputs $ inputs ;
     writeFile outf result ;
     putStrLn "done." ;
  }
  where
  outf = "merged.decls"
  fileNames = concat . intersperse ", " $ files
  
main :: IO()
main = do { args <- getArgs ; main_ args }

mainf args = main_ (words args)

main_ args = do {
   case args of
      ("--help" : _)  ->  putStrLn "declmerge file [file]+"
      files           ->  mergeFiles files 
   }
   
   
   