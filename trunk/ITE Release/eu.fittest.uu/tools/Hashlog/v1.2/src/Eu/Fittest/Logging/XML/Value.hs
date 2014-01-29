{- 

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | 

   This module provides a type representing serialized objects which
   are to be read from a log, and:

   * a parser to read from semi compressed raw log.
   * XML formater.   
     
-}

module Eu.Fittest.Logging.XML.Value(
     Value_(..),
     FieldSentence_(..),
     Field_(..),
     parseValue,
     parseSingleParValTy,
     buildXREFmap,
     getAllField_,
     getField_,
     getField__,
     getDeepAllField,
     getDeepField,
     getValType,
     getXID,
     maxXID,
     renumberXID,
     isNull,
     isUndefined,
     isUnserializable,
     isArray,
     isCollection,
     isDictionary,
     getArrayElems,
     getArrayElemsTypes,
     getCollectionElems,
     getCollectionElemsTypes,
     getDictionaryElems,
     getDictionaryKeysTypes,
     getDictionaryValsTypes
     --isSing
  )

where

import Debug.Trace
import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.IntMap
import Data.List
import Text.XML.Light
import Eu.Fittest.Logging.XML.Base
import Eu.Fittest.Logging.Compression.RawlogParser
import Eu.Fittest.Logging.Compression.Compress

-- | Type representing a serialized value or object in the log.
data Value_ = SingleVal_ { vVal::String, vTy:: String }       --single line value
            | Obj_   { ocname :: String, ofields::[Field_] }  --class name, fields
            deriving (Show,Eq)
   
{- ==============================================================
   The parser part. It parses from semi-compressed log.
============================================================== -}
     
-- | To parse a Value_ from a semi-compressed raw log.   
parseValue :: CompressedLogParser Value_
parseValue =  parseSingleParValTy <<| parseObj

-- | To parse a single paragraph Value_ from compressed log.
parseSingleParValTy :: CompressedLogParser Value_
parseSingleParValTy
   = 
   do { dict <- getDict_ ;
        p <- satisfy_ isParagraph `elseError` "Parsing a sigle-par value. Expecting a paragraph"; 
        case readVal dict p of 
           Just rv -> return rv
           _       -> fail "Parsing a single-par value. Fail on its sentence"
                      `withEvidence` [p]
   }
   where
   readVal dict (Par (Par_ [s])) 
        =   -- maybe monad
        do { (x,ty) <- splitValSentence s ;
              return (SingleVal_  { vVal=x, vTy=ty })
           }
   readVal dict (Par (Par_ _)) = Nothing 
   readVal dict (Par (XPar_ i))  
        = -- maybe monad
        do {
           sentences <- Data.IntMap.lookup i dict ;
           case sentences of
              [s] -> do { (x,ty) <- splitValSentence s ;
                          return (SingleVal_  { vVal=x, vTy=ty })
                        }
              _  -> fail ""
                       
        }

-- | to split a sentence of the form val:ty 
splitValSentence s 
    = 
    if couldNotSerialize then Just ("??", drop 3 s)         -- case ??
    else if isString     then Just (splitOnClosingQuote s)  -- case string
    else if not(Prelude.null ty_)  then Just (v, tail ty_)  -- other cases
    else Nothing
    where
    couldNotSerialize = take 2 s == "??"
    isString = take 1 s == "\""
    (v,ty_) = break (== ':') s    
    splitOnClosingQuote t = ('\"' : reverse b, tail(reverse a))
       where
       (a,b) = break (== '\"') (reverse (tail t))


-- | To parse an object from compressed log.
parseObj :: CompressedLogParser Value_
parseObj = do {
      dict <- getDict_ ;
      s@(Section _ tag_ elems) <- satisfy_ isSection `elseError` "Parsing an object. Expecting a section" ;
      case getTag dict tag_ of
         Nothing  -> fail "Parsing an object. Tag cannot be found in dict"
                     `withEvidence` [s]
         Just tag -> if isObjectTag tag then 
                         -- parse the elements of the section
                         case runCLparser parseFields dict elems of
                            Right (fields,[]) -> return (Obj_ { ocname = getType tag,
                                                               ofields = fields
                                                             }
                                                       )
                            _  -> fail "Parsing an object. Failing on subobjects"
                                  `withEvidence` [s]
                     else fail "Parsing an object. Failing on the syntax of the tag"
                          `withEvidence` [s]
   }
   where
   getTag dict (Tag_ t)  = Just t
   getTag dict (XTag_ i) = do {
                            t <- Data.IntMap.lookup i dict ;
                            if isSing t then return (head t) else fail ""
                           } 
   isObjectTag tag = take 2 tag == "O:"
   getType tag     = drop 2 tag
   

   
isSing [x] = True
isSing  _  = False   
   
-- intermediate type to represent a field
data FieldSentence_ = FnValTy_   String String String  -- name=3:int
                    | FnXref_    String Int            -- name=^1
                    | FnNextObj_ String                -- name=>    
         deriving Show
         
isFnNextObj_ (FnNextObj_ _) = True
isFnNextObj_  _ = False
         
-- another intermediate type to represent a field         
data Field_ = FieldValTy_  String String String  -- name,value,type
            | FieldXref_   String Int            -- name,ref
            | FieldObj_    String Value_         -- name,obj
        deriving (Show,Eq)
        
-- | to split  a setence of the form field=something        
splitFieldSentence s 
    =
    if Prelude.null s2 then Nothing
    else if separator == "=^" then return (FnXref_ fn (read . drop 2 $ s2))  -- case =^
    else if separator == "=>" then return (FnNextObj_ fn)                    -- case =>
    else -- maybe monad                                                      -- case =
         do { (v,ty) <- splitValSentence (tail s2) ;
              return (FnValTy_ fn v ty) }           
    where
    (fn,s2)   = break (== '=') s
    separator = take 2 s2   


-- to parser a paragraph containing (possibly multiple) fields
parseFieldsPar dict (Par (XPar_ i)) 
     = -- on maybe monad
     do {
       sentences <- Data.IntMap.lookup i dict ;
       sequence (Prelude.map splitFieldSentence sentences)
     }
      
parseFieldsPar dict (Par (Par_ sentences)) 
    = 
    sequence (Prelude.map splitFieldSentence sentences)

-- To parse a list of section fragments to a list fo fields.
-- This includes handling the =^ type of field.
parseFields = eof_ []
              <<|
              do { dict <- getDict_ ;
                   par  <- satisfy_ isParagraph `elseError` "Parsing a field-group. Expecting a paragraph" ;
                   case parseFieldsPar dict par of
                      Nothing -> fail "Parsing a field-group. Failing on a paragrah"
                                 `withEvidence` [par]
                      Just fields_  -> do {
                        (rest,lastf) <- return (splitAtLast fields_) ;
                        if isSing lastf && 
                           isFnNextObj_ (head lastf) 
                           then do { --special case, when last field is =^
                              fields <- return (Prelude.map convert rest) ;
                              FnNextObj_ fn <- return (head lastf) ;
                              obj <- parseObj ;  -- getting the next object
                              morefields <- parseFields ;  -- recursion
                              return (fields ++ [FieldObj_ fn obj] ++ morefields)  
                           }
                        else do {  --normal case, when last field is not =^
                           fields <- return (Prelude.map convert fields_) ;
                           morefields <- parseFields ;  -- recursion
                           return (fields ++ morefields)
                           }
                      }                             
                 }
    where                       
    convert (FnValTy_ fn v ty) = FieldValTy_ fn v ty
    convert (FnXref_ fn i)     = FieldXref_ fn i
    
    splitAtLast s = splitAt (length s - 1) s

{- ==============================================================
   The XML formater part
============================================================== -}

instance ConvertibleToXML Value_ where
   toXML (SingleVal_ { vVal=val, vTy=ty }) = mkElem "V" attrs []
      where
      attrs = attribs [("v",val),("ty",ty)]
   
   toXML (Obj_  {ocname=cname, ofields=fields }) 
      = 
      mkElem "O" [attrib1 "ty" cname] (Prelude.map toXML fields)
   
instance ConvertibleToXML Field_ where
   toXML (FieldValTy_ name val ty) =  mkElem "fd" 
                                           [attrib1 "n" name] 
                                           [toXML (SingleVal_ {vVal=val, vTy=ty})]
   
   toXML (FieldXref_  name ref) = mkElem "fd" 
                                     [attrib1 "n" name] 
                                     [mkElem "X" [attrib1 "to" (show ref)] []]
                                     
   toXML (FieldObj_   name obj) = mkElem "fd" [attrib1 "n" name] [toXML obj]

{- ==============================================================
   Some utilities to navigate through a value
============================================================== -}   
   
-- | This will build the mapping of subobjects and their IDs.
--
buildXREFmap :: Value_ -> [(Int,Value_)]
buildXREFmap (SingleVal_ _ _) = []
buildXREFmap obj = case id_ of
                   Just (FieldValTy_  _ id _) -> (read id, obj) : map_
                   _                          ->  map_
   where
   fields  = ofields obj
   id_     = find isXID fields
   subObjs = Data.List.map getVal . Data.List.filter isObj $ fields
   
   map_ = concat (Data.List.map buildXREFmap subObjs)
    
   isObj (FieldObj_ _ _) = True
   isObj _  =  False
   
   getVal (FieldObj_ _ val) = val
   
isXID (FieldValTy_  "I" _ "ID") = True
isXID _  =  False
   
-- | Return the integer ID of an object.
--
getXID :: Value_ -> Maybe Int   
getXID  (Obj_ {ocname = _ , ofields=fields }) 
     = 
     case id_ of
        Just  (FieldValTy_  _ id _) -> Just (read id)
        _                           -> Nothing        
   where
   id_ = find isXID fields   
   
-- | Return the greatest ID used in an object.   
maxXID val = maximum (0 : Data.List.map fst (buildXREFmap val))
  
-- | Renumber the IDs in an object by increasing them with k
--  
renumberXID  k v@(SingleVal_ _ _) = v
renumberXID  k v@(Obj_ {ocname = cn , ofields=fields }) 
   =
   (Obj_ {ocname = cn , ofields=fields2 })
   
   where
   id_  = getXID v
   
   fields2 = case id_ of
                Nothing -> [renumberXID_ k f | f <- fields]
                Just i  -> let
                           new_id  = FieldValTy_ "I" (show (i+k)) "ID"
                           fields3 = [ f | f <- fields, not . isXID $ f ]
                           in
                           new_id : [renumberXID_ k f | f <- fields3]
                           
renumberXID_ k field = case field of
   FieldXref_  n i  ->  FieldXref_ n (i+k)
   FieldObj_   n o  ->  FieldObj_  n  (renumberXID k o)
   _                ->  field
   
   
-- | Return the list of values associated to the field fn of the given
--   object. A FITTEST object may have multiple values associated to
--   the same field.
--    
getAllField_ :: [(Int,Value_)] -> String -> Value_ -> [Value_]
getAllField_ xREFmap fn (SingleVal_ _ _) = []
getAllField_ xREFmap fn (Obj_ {ocname = _ , ofields=fields }) 
    =
    concat [ fieldVal xREFmap f | f<-fields, fieldName f == fn ]
   
fieldName (FieldValTy_  n _ _) = n
fieldName (FieldXref_   n _)   = n
fieldName (FieldObj_ n _)      = n
   
fieldVal xREFmap field = case field of
   FieldValTy_ _ val ty  ->  [SingleVal_ val ty]
   FieldXref_ _ id       ->  case find ((== id) . fst) xREFmap of
                                 Just (_,o) -> [o]
                                 _          -> []
   FieldObj_ _ o   ->  [o]
   
-- | Return just a single value associated to a field in a given value. 
getField_ :: [(Int,Value_)] -> String -> Value_ -> Maybe Value_
getField_ xREFmap fn val = case getAllField_ xREFmap fn val of
                              []    -> Nothing
                              (o:_) -> Just o

-- | Same as getField_ , but it assumes that the field exists.   
getField__ :: [(Int,Value_)] -> String -> Value_ -> Value_                           
getField__  xREFmap fn val = fromJust .  getField_ xREFmap fn $ val

-- | Same as getAllField, but can get to a field that is deeper
--   in an object. Such a field is specified by a path.
getDeepAllField :: [(Int,Value_)] -> [String] -> Value_ -> [Value_] 
getDeepAllField xREFmap path val = worker path val
    where
    worker [] _             = []
    worker [fn] val         = getAllField_ xREFmap fn val
    worker (fn:therest) val = concat [ worker therest o | o <- subObjs ]
           where
           subObjs = getAllField_ xREFmap fn val
       
-- | Same as getField_, but can get to a field that is deeper
--   in an object. Such a field is specified by a path.       
getDeepField :: [(Int,Value_)] -> [String] -> Value_ -> Maybe Value_       
getDeepField xREFmap path val =  case getDeepAllField xREFmap path val of
                                   []    -> Nothing
                                   (o:_) -> Just o    
    
getValType (SingleVal_ {vVal=_ , vTy=ty})  = ty
getValType (Obj_ {ocname=c, ofields=_})    = c

isNull (SingleVal_ {vVal=_ , vTy=ty}) =  ty == "Null" 
isNull  _   =  False

isUndefined (SingleVal_ {vVal=_, vTy=ty}) =  ty == "void" 
isUndefined  _  =  False

isUnserializable (SingleVal_ {vVal=val, vTy=_})  =  val == "??"

isArray v      =  getValType v == "Array" 
isCollection v =  getValType v == "Collection" 
isDictionary v =  getValType v == "Dictionary" 

getArrayElems xREFmap a =  getAllField_ xREFmap "elem" a
getArrayElemsTypes xREFmap a = [getValType v | v <- getArrayElems xREFmap a]

getCollectionElems xREFmap c =  getAllField_ xREFmap "elem" c
getCollectionElemsTypes xREFmap c = [getValType v | v <- getCollectionElems xREFmap c]

getDictionaryElems xREFmap d = zip keys vals 
  where
  keys = getAllField_ xREFmap "key" d
  vals = getAllField_ xREFmap "val" d

getDictionaryKeysTypes xREFmap d  = [ getValType k | (k,_)<-getDictionaryElems xREFmap d]
getDictionaryValsTypes xREFmap d =  [ getValType v | (_,v)<-getDictionaryElems xREFmap d]
            
{- ==============================================================
   Examples for test
   
   use e.g. 
     uncurry (topRunCLparser parser) exPar1  to test parsers
     putStr . ppElement . toXML . uncurry (topRunCLparser parser) $ exPar1  to test XML converter
============================================================== -}
     
exPar0 = (dic,log2) 
   where
   (dic,log1) = strCompress "%<P  %>"  
   log2 = decompressTimeStamp log1
   
   
exPar1 = (dic,log2) 
   where
   (dic,log1) = strCompress "%<P %<{ 3:int }%> %>"  
   log2 = decompressTimeStamp log1
   
exPar2 = (dic,log2) 
   where
   (dic,log1) = strCompress "%<P %<{ \"hello\":String }%> %>"  
   log2 = decompressTimeStamp log1
            
exPar3 = (dic, tail log2) 
   where
   (dic,log1) = strCompress "%<P %<{ 3:int }%> %> %<P %<{ 3:int }%> %>"  
   log2 = decompressTimeStamp log1
   
exPar4 = (dic,log2) 
   where
   (dic,log1) = strCompress "%<P %<{ 3:int }%> %<{ 4:int }%> %>"  
   log2 = decompressTimeStamp log1   
   
exPar5 = (dic,log2) 
   where
   (dic,log1) = strCompress "%<P %<{ }%>  %>"  
   log2 = decompressTimeStamp log1   

exObj1 = (dic,log2) 
   where
   (dic,log1) = strCompress "%<S 0:0 \"O:package::class\" %<P %<{ x=100:int}%>%>%>"  
   log2 = decompressTimeStamp log1 

exObj2 = (dic,log2) 
   where
   (dic,log1) = strCompress "%<S 0:0 \"O:packageA.B.C::class\" %<P %<{ x=100:int}%> %<{ y=??:pck.A.B::class }%> %>%>"  
   log2 = decompressTimeStamp log1 

exObj3 = (dic,log2) 
   where
   (dic,log1) = strCompress "%<S 0:0 \"O:packageA.B.C::class\" %<P %<{ x=100:int}%>%> %<P%<{ y=^1 }%> %>%>"  
   log2 = decompressTimeStamp log1 

exObj4 = (dic,log2) 
   where
   (dic,log1) = strCompress "%<S 0:0 \"O:packageA.B.C::class\" %<P %<{ next=> }%>%>  %<S 0:1 \"O:packageA.B.C::class\" %<P %<{ next=null:Null }%>%>%> %<P %<{ x=100:int}%>%> %>"  
   log2 = decompressTimeStamp log1       

            
            
            