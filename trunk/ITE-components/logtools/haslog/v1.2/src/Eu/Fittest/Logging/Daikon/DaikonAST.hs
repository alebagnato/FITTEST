{- 

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 

   This module provides data types representing Daikon logs, along 
   with pretty printers.
   
   The representation is currenyly incomplete. See docs below.
   
     
-}

module Eu.Fittest.Logging.Daikon.DaikonAST(
      Ppt(..)
    , PptType(..)
    , VarDecl(..)
    , VarKind(..)
    , Flag(..)
    , DaikonRecord(..)
    , DaikonValue(..)
    , DaikonSimpleValue(..)
    , DaikonType(..)
    , SimpleType(..)
    , DaikonPP(..)
   )

where

import Data.List

{- =============================================================
   Data types that faithfully represent Daikon log format1  
============================================================= -}

-- | Representing Daikon's Program Point. Not all types of Ppt's info
--   are supported.
data Ppt = Ppt { -- | Proram point's name
                 pptName  :: !String,
                 -- | Program point's type
                 pptType  :: !PptType,
                 -- | Specification of parent program point, if any.
                 -- The parent is relation type (parent/user), the name of
                 -- the parent ppt, and a unique int-ID of the relation.
                 pptParent :: !(Maybe (String,String,String)),
                 -- | variables delcared in this ppt.
                 varDecls :: ![VarDecl]
               }
               deriving (Eq,Show)
               
-- | Representing different types of program point. Not all types are supported.       
data PptType = 
          Point_
        | Class_
        | Object_    
        | Enter_
        | Exit_
        -- | Subexit_   .. not supported
        deriving (Eq,Show)
        
-- | Representing a declaration of a variable. Not all kinds of variable-info
--   are currently supported.
--
data VarDecl = VarDecl {
                  vName :: !String  ,
                  -- | The enclosing variable, if any.
                  vEnclosingVar :: !(Maybe String),
                  -- vRelativeName :: Maybe String,
                  
                  -- | Specifying the kind of the variable (variable, field, array, etc)
                  varKind :: !VarKind,
                  -- | The original type of the variable.
                  varOrgType :: !String,
                  -- | The daikon-type that represents this variable in Daikon.
                  --   (int, double, java.lang.string etc).
                  varType :: !DaikonType,
                  vComparability :: !String,    
                  -- | The parent program point, if any. The format is
                  --   name of parent ppt, relation ID.
                  vParent :: !(Maybe (String,String)),                  
                  arrayDim :: !(Maybe Int),
                  vFlags :: ![Flag]
                }
                deriving (Eq,Show)
                
-- | Representing different kinds of variable in Daikon. Not all kinds are
--   supported.              
data VarKind = 
       VkVariable
     -- | VkField  not supported now
     | VkReturn
     | VkArray   
     | VkFunction
     deriving (Eq,Show)
     
data Flag = IsClassName | IsParam  deriving (Eq,Show)  
     
                 
-- | Representing Daikon's data trace record.                 
data DaikonRecord =  DaikonRecord {
       -- the name of the program point
       drPptName :: !String,
       drNonce   :: !String,
       -- values logged at that point in: var-name, value, modified-flag
       drValues  :: ![(String,DaikonValue,Int)]
    }                  
    deriving (Show,Eq)
    
-- | Representing a Daikon value of a variable. Currently arrays can only be
--   one dimensional. (Daikon does not support multi dimensional array anyway)  
data DaikonValue = Single !DaikonSimpleValue
                 | Multiple ![DaikonSimpleValue]
                 deriving (Show,Eq)
              
data DaikonSimpleValue = Val1 !String
                | Null
                | Nonsesical
                deriving (Show,Eq)

-- | Representing Daikon-types of variables. Currently arrays can only be
--   one dimensional. (Daikon does not support multi dimensional array anyway)             
data DaikonType = SimpleType_  !SimpleType
                | Array_       !SimpleType
                deriving (Show,Eq) 
                
data SimpleType = Int_ 
           | Bool_
           | Double_
           | String_ 
           | Hash_
           deriving (Show,Eq)

{- =============================================================
   Pretty printer
============================================================= -}  
  
class DaikonPP a where
   dpp      :: a -> String
   dpp_     :: a -> String -> String  -- dpp_ x tail = dpp x ++ tail
   dprintPp :: a -> IO()
   dprintPp a = putStr (dpp a)
   dpp a      = dpp_ a ""
   
indent_ k  = replicate k ' ' 
indent__ k = string_ (indent_ k)
string_ s  = (\tail-> s ++ tail)
f ++++ g   = (\tail-> f (g tail)) -- just function composition
infixr 9 ++++
concat_ [] = id
concat_ (fs:rest) = fs ++++ concat_ rest  
   
instance DaikonPP DaikonRecord where
   dpp_ DaikonRecord {
       drPptName =name ,
       drNonce = nonce,
       drValues = vals 
       }
      =
      concat_ . intersperse (string_ "\n")
      $
      [ string_ name, 
       string_ "this_invocation_nonce",
       string_ nonce ] 
       ++
      ( map string_ . concatMap f $ vals)
      
      where
      f (vname,val,modified) = [vname, g val, show modified]
      g (Single v)    = g2 v
      g (Multiple vs) = "[" 
                        ++
                        concat (intersperse " " (map g2 vs))
                        ++
                        "]"      
      g2 (Val1 v)    =  v                       
      g2  Null       = "null"
      g2   Nonsesical = "nonsensical"
       
instance DaikonPP VarKind where   
    dpp_ d = case d of
      VkVariable  -> string_ "variable"
      VkReturn    -> string_ "return"
      VkArray     -> string_ "array" 
      VkFunction  -> string_ "function"
  
instance DaikonPP DaikonType where
    dpp_ (SimpleType_  ty) = dpp_ ty
    dpp_ (Array_ ty)       = dpp_ ty ++++ string_ "[]"
       
instance DaikonPP SimpleType where  
    dpp_ ty = case ty of
        Int_     ->  string_ "int"
        Bool_    ->  string_ "boolean"
        Double_  ->  string_ "double"
        String_  ->  string_ "java.lang.String"
        Hash_    ->  string_ "hashcode"

instance DaikonPP Flag where
   dpp_ IsClassName = string_ "classname"
   dpp_ IsParam     = string_ "is_param"
   
instance DaikonPP VarDecl where
    dpp_ (VarDecl {
           vName = vn,
           vEnclosingVar = parent,
           varKind = vkind,
           varOrgType = orgty,
           varType = dty,
           vComparability = c,  
           vParent = parentPpt ,       
           arrayDim = dim,
           vFlags = fs
        })
       =
       indent__ 3 ++++ string_ "variable "  ++++ string_ vn ++++ string_ "\n"
       ++++
       ( concat_ . intersperse (string_ "\n") . map (indent__ 6 ++++)
         $
         [ string_ "var-kind "  ++++ dpp_ vkind ] 
         ++ enclosingvar 
         ++ arraydim ++
         [ string_ "dec-type " ++++ string_ orgty,
           string_ "rep-type " ++++ dpp_ dty ,
           string_ "comparability " ++++ string_ c   ]
         ++ parentPpt_ 
         ++ flags 
       )
       where
       
       enclosingvar  =  case parent of
                             Nothing -> []
                             Just w  -> [string_ ("enclosing-var " ++ w)]    
       arraydim = case dim of
                     Nothing -> []
                     Just d  -> [string_ ("array " ++ show d)] 
       flags = if null fs then []
               else [ string_ "flags "  
                     ++++
                     (concat_ . intersperse (string_ " ") . map dpp_ $ fs) ]
       parentPpt_ = case parentPpt of
                    Just (prnt,id)  ->  [string_ ("parent " ++ prnt ++ " " ++ id) ]
                    _  ->  []

instance DaikonPP PptType where
   dpp_ p = case p of
      Point_   ->  string_ "point"
      Class_   ->  string_ "class"
      Object_  ->  string_ "object"
      Enter_   ->  string_ "enter"
      Exit_    ->  string_ "exit"
      
instance DaikonPP Ppt where
   dpp_ (Ppt { pptName = name,
              pptType = pty,
              pptParent = parent,
              varDecls = vdecls
            })
      =
      concat_ . intersperse (string_ "\n")
      $
      [ string_ ("ppt " ++ name) ,
        string_ "ppt-type " ++++ dpp_ pty ]
      ++ parent_ ++
      map dpp_ vdecls
      
      where
      parent_  =  case parent of 
                   Just (type_,prnt,id) -> [string_ ("parent " ++ type_ ++ " " ++ prnt ++ " " ++ id)]
                   _   ->  []
      
       
   
