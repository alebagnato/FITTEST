{- 

Author: Wishnu Prasetya

Copyright 2013 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 
   This module provides functions to parse Daikon oracle reports (.txt) and convert
   them in our internal oracles representation.
-}

{-# LANGUAGE OverloadedStrings #-}

module Eu.Fittest.Oracles.Parser.DaikonParser

where

import Eu.Fittest.Oracles.Ast.OrcsDataType

-- import Prelude hiding (takeWhile,readFile)
import Data.Attoparsec.Text 
import Control.Applicative
import Control.Monad
import Data.Char
import qualified Data.Text as T
import Text.Show
import qualified Data.Text.IO as T_IO
import Data.Maybe
import Data.List
import qualified Data.Map as Map

import Debug.Trace

type Line = String

-- ============================================================
-- Top level functions, and its direct workers
-- ============================================================

-- top level function, to parse multiple Daikon oracles reports;
-- additionally we also pass a filter function, to drop oracles
-- for which the filter returns true. E.g. this can be used to
-- drop oralces that have too few witnesses.
parseDaikonOrcsReports :: [String] 
                          -> ((Oracle2, Maybe Int, Maybe (OracleVerdict,String)) -> Bool)
                          -> IO OracleSuite 
parseDaikonOrcsReports filenames isNotWantedOracle = do {
   sgs <- sequence [ basicparseFromFile f | f <- filenames ] ;
   return . sortSuite . regroup . filterOrcs . concat $ sgs
}
   where
   regroup subgroups = Map.assocs map
       where
       map = worker Map.empty subgroups
       
   worker map [] = map
   worker map ((event,[]):rest) = worker map rest
   worker map ((sg@(event,oracles)):rest) = worker map' rest
       where
       gname = getGroupName event
       map'  = if Map.member gname map 
                  then Map.adjust (sg:) gname map
                  else Map.insert gname [sg] map


   getGroupName (ET_AppEvent targetId _)   =  targetId
   getGroupName (ET_MethodSubcase mname _) =  mname   
   
   -- to drop oracles that have too few witnesses   
   filterOrcs [] = []
   filterOrcs ((e,orcs):rest) = case sg' of
                                           (_,[])  ->  filterOrcs rest
                                           _       -> sg' : filterOrcs rest
        where
        sg' = (e, [o | o<-orcs, not (isNotWantedOracle o)])
        
   -- sorting the groups alphabetically
   sortSuite s = sortBy ordering s
   ordering (gname1,sgs1) (gname2,sgs2) = compare gname1 gname2
         
     
basicparseFromFile :: String -> IO[OracleSubGroup]
basicparseFromFile filename = do {
        content <- T_IO.readFile filename ;
        case parse basicparser content of
           Done _ result -> return result
           _             -> error "parser fails..."             
    }
    
-- just for testing:
testparseFromFile p fname = do  {
        content <- T_IO.readFile fname ;
        return . parse p $ content             
    }  
    
basicparser :: Parser [OracleSubGroup]   
basicparser =  do {
    pHeader ;
    sg <- pSubGroups ;
    return sg
  }    

-- ============================================================
-- Utilities
-- ============================================================
  
skipSpaces = skipSpace 
  
untilEndOfLine = do {
     skipWhile ('\n' /=) ;
     endOfLine ;
   }

pHeader :: Parser()
pHeader =  pLineSep <|> (untilEndOfLine *> pHeader)

pLineSep :: Parser()
pLineSep = do {
     string "===" ;
     untilEndOfLine ;
     -- trace ">>parsing linesep" $ return () ;
   }

-- horizontal separator: at least one space
pSpaceSep :: Parser()   
pSpaceSep = do { space1 ; many space1 ; return () }

space1 = satisfy (\c-> c==' ' || c=='\t')

-- ============================================================
-- sug-groups parser
-- ============================================================
   
pSubGroups =  many ( pSubGroup <* pLineSep) 
   
-- pSubGroup :: Parser OracleSubGroup
pSubGroup = do {
     header <- pSubGroupHeader ;
     trace (">>parsing subgroup-header " ++ show header) $ return () ;
     pVars <|> return () ;
     -- this doe not work; probably due some specific way "try" is implemented: (try pModifiedVars) ;
     pModifiedVars <|> return () ;
     --trace (">>parsing sg vars " ++ show token) $ return () ;
     orcs1 <- (pUnmodifiedVars <|> return []) ;
     orcs2 <- pOracles ;
     subgroup <- return (f header orcs1 orcs2) ;
     return subgroup
   }            
  where
  
  f header orcs1 orcs2 = (interpretEventName name , orcs)
     where
     (name,qualifier,numOfSample) = header
     orcs = [ (categorize  o, Just numOfSample, Nothing) | o <- orcs1 ++ orcs2 ]
     categorize o = if qualifier == "EXIT1" then Post o else Pre o
        
-- interpret a given eventname to map it to the corresponding representation
-- of event-type. E.g. 
--
--     change_IDY_addressState --> ET_AppEvent IDY_addresState change
--     Game.Update_565_566_568 --> ET_MethodSubcase Game.Update [565,566,568]
-- 
interpretEventName :: String -> EventType
interpretEventName name = if isLLO
                          then ET_MethodSubcase prefix (getNodeIds . tail $ suffix_)
                          else ET_AppEvent targetId prefix
     where
     (prefix,suffix_)    = break (== '_') name
     targetId = if null suffix_
                   then "targetId??"
                   else tail suffix_
                   
     isLLO = not (null suffix_) && isDigit (suffix_!!1)   
     
     getNodeIds "" = []
     getNodeIds s  = if null rest then [read x] else (read x : getNodeIds (tail rest))
        where
        (x,rest) = break (== '_') s
      

      
pPositiveInt :: Parser Int 
pPositiveInt = do { x <- digit ; s <- many digit ; return . read $ (x:s) }
  

-- to parse e.g. itemclick_ButtonBar0(..):::EXIT1  
pEventName :: Parser (String, String)  
pEventName = do {
      name <- pIdentifier ; -- takeTill (== ':') ;
      string "(..)" <|> return "" ;
      string ":::"  ;
      qualifier <- (string "OBJECT" <|> string "POINT" <|> string "ENTER" <|> string "EXIT1") ;
      return (name, T.unpack qualifier) 
    } 
    
pSubGroupHeader :: Parser (String, String, Int)  
pSubGroupHeader = do {
      (name,qualifier) <- pEventName ;
      skipSpaces ;
      n <- pPositiveInt ;
      untilEndOfLine ;
      -- trace ">>ending pSubGroup..." $ return () ;
      return (name,qualifier,n)
    }


-- to parse variables listed in an oracle-subgroup entry; for now we ignore them    
pVars = do {
   skipSpaces ;
   string "Variables:" ;
   vars <- many (pSpaceSep *> pVar) ;
   {- skip the rest of the line -} untilEndOfLine ;
}

-- to parse an identifier; note that the "rest"-part never fails
pIdentifier :: Parser String
pIdentifier = do {
   c1   <- satisfy isAlpha ;
   -- note: allowing \' may confuse the parsing of violations... :
   rest <- many (satisfy (\c-> isAlphaNum c || c `elem` "._\'")) ;
   return  (c1:rest)
   }

-- parsing a single variable
pVar :: Parser String
pVar = do {
        v <- pIdentifier ;
        {- trace (">>pVar " ++ show v) -} return v
      }
      
-- parsing an operator
pOperator op = do { skipSpaces ; string op ; return () }

pUnmodifiedVars :: Parser [Oracle]
pUnmodifiedVars = do {
       {- zero or more leading space -} many space1 ;
       string "Unmodified variables:" ;
       -- trace ">>pUnmodifiedVar" (return ()) ;
       vars <- many (pSpaceSep *> pVar) ;
       {- ignore the rest of the line -} untilEndOfLine ;
       return [ O_unModified . VariableName . breakName $ name | name <- filterVar vars]
    }
    where
    -- for now, dropping arg-vars because our daikon translator
    -- simply copy them to the exit-point.
    filterVar vars = filter (\v-> not("arg" `isPrefixOf` v)) vars
    
-- parsing line specifying modified vars; for now they will be ignored
pModifiedVars :: Parser ()
pModifiedVars = do {
       {- zero or more leading space -} many space1 ;
       string "Modified variables:" ;
       -- trace ">>pModifiedVar" (return ()) ;
       vars <- many (pSpaceSep *> pVar) ;
       {- ignore the rest of the line -} untilEndOfLine ;
       return ()
    }

    
-- breakName a.b.c = [a,b,c] 
breakName "" = []
breakName s  = if null rest then [prefix] else prefix : breakName (tail rest)
       where
       (prefix,rest) = break (== '.') s 
    
    
-- ============================================================
-- parser for oracles
-- ============================================================
    
-- to parse multiple oracle lines; terminated by a === line  
pOracles :: Parser [Oracle]  
pOracles = do { orcs <- pOracles_ ;
                return  [ fromJust o | o <- orcs, isJust o ]
              }
              
pOracles_ = string "===" *> return []
            <|>
            do { o <- do { {- zero or more leading space -} many space1 ; pOracleLine } ;
                 z <- pOracles_ ;
                 return (o:z) }
   
-- to parse a single oracle line, and just skip the line if it cant recognize the oracle   
pOracleLine :: Parser (Maybe Oracle)
pOracleLine = do { o <- pOracle ;
                   {- ignore the rest of the line -} untilEndOfLine ;
                   return (Just o) 
                  }
              -- to skip oracle-like that we can't recognize... :
              <|> do { untilEndOfLine ; return Nothing }
          
-- to parse a single oracle 
pOracle :: Parser Oracle
pOracle = pRelationOrc
          <|> pIsSortedOrc
          <|> pIsOneOfOrc
          <|> pIsConstantOrc

pRelationOrc :: Parser Oracle
pRelationOrc = do {
     t1 <- pTerm ;
     {- zero or more space -} many space1 ;
     operator <- pRelationOp ;
     {- zero or more space -} many space1 ;
     t2 <- pTerm ;
     return (O_Relation operator t1 t2)
    }
    
pRelationOp = 
   pLesserOrGreaterOp
   <|>  do { string "=="  ; return EQ_ }  
   <|>  do { string "!="  ; return NotEQ_ }  
   
pLesserOrGreaterOp = 
   do { string "<=" ; return LTE_ }
   <|>  do { string "<"  ; return LT_ }     
   <|>  do { string ">=" ; return GTE_ } 
   <|>  do { string ">"  ; return GT_ }      
   
pIsConstantOrc :: Parser Oracle   
pIsConstantOrc = do { 
   t <- pOldVarName <|> pVarName ;
   space1 ;
   string "has only one value" ;
   return (O_isConstant t)
  }   
  
pIsSortedOrc :: Parser Oracle
pIsSortedOrc = do { 
   t <- pOldVarName <|> pVarName ;
   space1 ;
   string "sorted by" ;
   space1 ;
   rel <- pLesserOrGreaterOp ;
   return (O_SortedBy rel t)
  }   
 
pIsOneOfOrc :: Parser Oracle  
pIsOneOfOrc = do { 
   t <- pOldVarName <|> pVarName ;
   space1 ;
   string "one of" ;
   space1 ;
   set <- pConstSet ;
   return (O_oneOf t set)
  } 
  
-- ============================================================
-- parser for terms
-- ============================================================
 
pTerm :: Parser Term
-- order sensitive :(  ...
pTerm = pBinaryTerm <|> pAtomicTerm 

-- recursive term:            
pBinaryTerm :: Parser Term
pBinaryTerm = do { t1 <- pAtomicTerm ; 
                   pSpaceSep ;
                   op <- pBinaryOp ; pSpaceSep ;
                   t2 <- (pBinaryTerm <|> pAtomicTerm) ;
                   return (Binary op t1 t2)
                  }
    
pBinaryOp = 
   do { string "+" ; return Plus_ }
   <|>  do { string "%"  ; return Modulo_ }   
   
-- non recursive terms 
pAtomicTerm :: Parser Term
pAtomicTerm = do { 
   ( -- becareful... this is order sensitive
     (do { string "null" ; return LitNull } )
     <|> pConstNumber  
     <|> pConstBool 
     <|> pConstString    
     <|> pOldVarName
     <|> pSizeOf
     <|> pVarName
     <|> pConstList
     <|> pConstSet
    ) 
   }   
             
pVarName :: Parser Term  
pVarName = do {
     n <- pVar ;
     return . VariableName . breakName  $ n 
   }
   
   
   
pOldVarName :: Parser Term
pOldVarName = do {
      string "orig(" ;
      n <- pVar ;
      string ")" ;
      return . OldVariableName . breakName  $ n 
   }
  
pConstNumber :: Parser Term
pConstNumber = do {
     n <- number ;
     case n of
        I i -> return . LitInt . fromInteger $ i
        D d -> return . LitDouble $ d 
  }     
           
pConstBool :: Parser Term        
pConstBool = (\s-> LitBool True) <$> string "true"
             <|>
             (\s-> LitBool False) <$> string "false"
             
pConstString :: Parser Term
pConstString = do {
       str <- scan "START" fsm ;
       if str == "" then fail "" 
                    else return . LitString . stripQuotes . T.unpack $ str
    }
    where
    fsm "START"  '\"'  = Just "INSIDE"
    fsm "START"  _     = Nothing
    fsm "INSIDE" '\\'  = Just "ESCAPING"
    fsm "INSIDE" '\"'  = Just "DONE"
    fsm "INSIDE" _     = Just "INSIDE"
    fsm "ESCAPING" _   = Just "INSIDE"
    fsm "DONE" _       = Nothing
    
    stripQuotes s = dropLast . drop 1 $ s
    dropLast [x]   = []
    dropLast (x:s) = x : dropLast s

pConstList :: Parser Term
pConstList =  LitIntList <$> pConstList__ pInt
              <|> LitDoubleList <$> pConstList__ pDouble
              <|> LitBoolList   <$> pConstList__ pBool
              <|> LitStringList <$> pConstList__ pString
              
pConstSet :: Parser Term
pConstSet =  LitIntSet <$> pConstSet__ pInt
              <|> LitDoubleList <$> pConstSet__ pDouble
              <|> LitBoolList   <$> pConstSet__ pBool
              <|> LitStringList <$> pConstSet__ pString              
    
pInt = do {
             x <- number ;
             case x of 
               I j -> return (fromInteger j)
               _   -> fail "pInt expects an integer"    
           } 
    
pDouble = do {
                x <- number ;
                case x of 
                  D d -> return d
                  _   -> fail "pDouble expects a double"    
              } 
              
pBool = do { LitBool b <- pConstBool ; return b }
    
pString = do { LitString s <- pConstString ; return s }
         
pConstList__ = pConstListLike__  "["  "]" 
pConstSet__  = pConstListLike__  "{ "  " }" 
         
pConstListLike__ opening closing pElem = do {
     string opening ;
     worker ;
  }
  where
  worker = string closing *> return []
           <|> do { i <- pElem ; string closing ; return [i] }
           <|> do {
                 i <- pElem ;
                 string "," ;
                 space1 ;
                 s <- worker ;
                 return (i:s)
               }
    
pSizeOf :: Parser Term    
pSizeOf = do {
     v <- pVarName ;
     space1 ;
     string "elements" ;
     return . SizeOf $ v
  }  
  

-- ============================================================
-- violations, and a function to mark an oracle suite with
-- informtion from a list of violations
-- ============================================================ 
  
-- a type representing a violation line; it specifies which event
-- is being violated, which oracle, and an info string
type ViolationLine = (EventType, Oracle2, String)

markOrcsSuiteWithViolations :: [ViolationLine] -> OracleSuite -> OracleSuite
markOrcsSuiteWithViolations violations orcsSuite = map markGroup orcsSuite
   where
   
   getViolations e = [ (o,i) | (e',o,i) <- violations, e==e' ]
   
   markGroup (groupName, subgroups) = (groupName, map markSG subgroups)
   
   markSG (e,orcs) =  case getViolations e of
                        []    -> (e,orcs)
                        vios  -> (e, map (markO vios) orcs)
                        
   -- mark each oracle, if it already marked as fail, dont change
   markO vios z@(o,k,Just (Violated,info)) = z
   -- else we check if o is violated; if so, we add the corresponding flag and info
   markO vios (o,k,verdict) = worker vios
      where
      worker [] = (o,k,verdict)
      worker ((o',info):rest) = if o==o' then (o,k, Just (Violated,info)) 
                                         else  worker rest
       
  
-- ============================================================
-- parser of violation reports
-- ============================================================ 



parseViolationsFromFiles :: [String] -> IO [ViolationLine]
parseViolationsFromFiles files = do {
     violations <- sequence [basicparseViolationsFromFile f | f <- files] ;
     return . concat $ violations
  }

-- parsing a single violation file
basicparseViolationsFromFile :: String -> IO[ViolationLine]
basicparseViolationsFromFile filename = do {
        content <- T_IO.readFile filename ;
        case parse pViolations (T.append content (T.pack "===")) of
           Done _ result -> return result
           _             -> error "parser fails..."             
    }
   
-- ideally, i just want to do many pViolationLine_ ; but this does not work as
-- the parser seems to have a problem recognizing end of input; so we add an extra
-- === string to the input to mark the end of it, and parse multiple violation-line
-- like this.
-- Hackish :(
--
pViolations :: Parser [ViolationLine]
pViolations = do { string "==="  ; return [] }
              <|> do { o <- pViolationLine_ ;
                       s <- pViolations ;
                       case o of 
                         Nothing -> return s
                         Just o' -> return (o':s)
                     }

pViolationLine_ =   pViolationLine <|> do { untilEndOfLine ; return Nothing }

pConstantValue = pConstNumber <|> pConstBool <|> pConstString    

-- this tries to parse a violation-line, and returns a tuple (ety,
pViolationLine :: Parser (Maybe ViolationLine)
pViolationLine = do {
   string "At ppt" ;
   pSpaceSep ;
   (eventName,qualifier) <- pEventName ;
   string ", Invariant '" ;
   o <- pOracle ;
   string "' invalidated by sample" ; 
   pSpaceSep ;
   varName <- pIdentifier ;
   string "=" ;
   violatingValue <- pConstantValue ;
   string ": " ;
   info <- Data.Attoparsec.Text.takeWhile ('\n' /=) ;
   endOfLine ;
   return . Just . categorize . fixPreCondCheckedAtPost. fixOneOfViolation $ (eventName, qualifier, o, varName, violatingValue, T.unpack info)
}
   where
   -- to fix wrongly reported violation of this type: x one of {1,2,3} is violated by x=2
   -- It should be x one of {1,3} is violated by x=2 (I have cross check this in the report.txt)
   --
   fixOneOfViolation (e,q, O_oneOf x set, v, value, info) = (e,q, O_oneOf x (fix set), v, value,info)
      where
      violatingvalue_ = show value
      -- fixing the oneOf set
      fix (LitIntSet s)    = LitIntSet [v | v<-s , show v /= violatingvalue_]
      fix (LitBoolSet s)   = LitBoolSet [v | v<-s , show v /= violatingvalue_]
      fix (LitStringSet s) = LitStringSet [v | v<-s , show v /= violatingvalue_]
      fix (LitDoubleSet s) = LitDoubleSet [v | v<-s , show v /= violatingvalue_]
      fix t = t
      
   fixOneOfViolation r = r
   
   -- fix violation of the form, e.g. event(..):::EXIT1 ... old(x)==0 is violated ...
   -- this should be a violation to a pre-condition, so should be:
   --     event(..):::ENTER ... old(x)==0 is violated ...
   --
   fixPreCondCheckedAtPost r@(e,"EXIT1",o,v,value,info) = 
       if onlyReferToOldVars o then (e,"ENTER", makeOldNew o, v, value, info) else r

   fixPreCondCheckedAtPost r = r
   
   -- categorize if the violated oracle is a pre or post-condition
   categorize (e,q,o,v,value,info) = (interpretEventName e, o', info') 
      where
      o' = if q == "EXIT1" then Post o else Pre o
      info' = "violated by " ++ v ++ " = " ++ show value ++ ", " ++ info
   
-- check if an oracle only refer to "old" variables
onlyReferToOldVars o = check o
  where
  check (O_Relation _ t1 t2) = checkt t1 && checkt t2
  check (O_SortedBy _ t) = checkt t
  check (O_oneOf t1 t2)  = checkt t1 && checkt t2
  check (O_isConstant t) = checkt t
  check (O_unModified t) = checkt t  

  checkt (VariableName _)    = False
  checkt (OldVariableName _) = True
  checkt (SizeOf t)  = checkt t
  checkt (Binary _ t1 t2) = checkt t1 && checkt t2
  checkt _  = True

-- to replace reference to old-vars with new-vars
makeOldNew o = rep o
  where
  rep (O_Relation r t1 t2) = O_Relation r (rept t1) (rept t2)
  rep (O_SortedBy r t) = O_SortedBy r (rept t)
  rep (O_oneOf t1 t2)  = O_oneOf (rept t1) (rept t2)
  rep (O_isConstant t) = O_isConstant (rept t)
  rep (O_unModified t) = O_unModified (rept t) 
  
  rept (OldVariableName v) = VariableName v
  rept (SizeOf t) = SizeOf (rept t)
  rept (Binary op t1 t2) = Binary op (rept t1) (rept t2)
  rept t = t
  
   

