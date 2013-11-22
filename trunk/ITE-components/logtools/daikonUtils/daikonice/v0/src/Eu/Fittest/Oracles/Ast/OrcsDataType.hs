{- 

Author: Wishnu Prasetya

Copyright 2013 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{-# LANGUAGE FlexibleInstances #-}

{-|
    This module define the data types to represent a grouped list of Daikon oracles.
    The representation is customized towards our the way Haslog translates FITTEST
    logs to Daikon logs.
-}
module Eu.Fittest.Oracles.Ast.OrcsDataType 

where

data Term =
         VariableName [String]     -- e.g. x.a.b
       | OldVariableName [String]  -- old(x.a.b)
       | MethodParam    Int        -- to refer to k-th method's parameter
       | OldMethodParam Int
       | LitNull       
       | LitInt        Int
       | LitBool       Bool
       | LitString     String
       | LitDouble     Double
       | LitIntList    [Int]
       | LitBoolList   [Bool]
       | LitStringList [String]
       | LitDoubleList [Double]
       | LitIntSet    [Int]
       | LitBoolSet   [Bool]
       | LitStringSet [String]
       | LitDoubleSet [Double]
       | SizeOf        Term        -- size of an array
       | Binary ArithBinOp Term Term
       deriving (Eq,Show)
      
data ArithBinOp = Plus_ | Modulo_ deriving (Show,Eq)       
data Relation_  = GT_ | GTE_ | LT_ | LTE_ | EQ_ | NotEQ_ | EQUIV_ deriving (Show,Eq)
      
data Oracle =
        O_Relation Relation_ Term Term
      | O_SortedBy Relation_ Term  -- e.g. a is sorted by <
      | O_oneOf Term Term          -- e.g. x one of {1,99}
      | O_isConstant Term          -- to say that x never changes
      | O_unModified Term          -- to say that t = old(t)
      | O_Pattern String             -- algebraic pattern oracle  
      deriving (Eq,Show)
      
-- wrap an oracle with indication whether it is a pre or post-condition      
data Oracle2 = Pre Oracle | Post Oracle | Pattern Oracle deriving (Eq,Show)
      
data EventType =
       ET_AppEvent String String     -- target-id followed by event type, e.g. click
     | ET_MethodSubcase String [Int] -- method's name following by a sequence of node-ids;
                                     -- empty means "true" subcase.
     deriving (Eq,Show)
     
-- This is to descibre the set of oracles that belong to the same sub-case or app-event.
-- For each oracle we indicate if it is a pre- or post-condition. We may also show how 
-- many witnesses we have, and whether it has been violated or not.   
type OracleSubGroup = (EventType, [(Oracle2, Maybe Int, Maybe (OracleVerdict,String))])  

data OracleVerdict = Pass | Violated | Undecided deriving (Eq,Show)

-- this is to describe the sets of oracles that belong to the same method or event-target
type OracleGroup = (String, [OracleSubGroup]) 

-- this is to describe the suite of all oracles
type OracleSuite = [OracleGroup]    

class CountingOracles a where
   countOrcs :: a -> Int
   countViolations :: a -> Int
   countUndecideds :: a -> Int
   
instance CountingOracles OracleSubGroup where
    countOrcs (_,orcs) = length orcs
    countViolations (_,orcs) = sum [ f v | (_,_,v) <- orcs]
        where
        f (Just (Violated,_)) = 1
        f _  =  0
        
    countUndecideds  (_,orcs) = sum [ f v | (_,_,v) <- orcs]
        where
        f (Just (Undecided,_)) = 1
        f _  =  0
        
instance CountingOracles OracleGroup where
    countOrcs (_,subgroups)       = sum [ countOrcs sg | sg <- subgroups ]
    countViolations (_,subgroups) = sum [ countViolations sg | sg <- subgroups ]
    countUndecideds (_,subgroups) = sum [ countUndecideds sg | sg <- subgroups ]
    
instance CountingOracles OracleSuite where
    countOrcs groups       = sum [ countOrcs g | g <- groups ]
    countViolations groups = sum [ countViolations g | g <- groups ]
    countUndecideds groups = sum [ countUndecideds g | g <- groups ]

                                 