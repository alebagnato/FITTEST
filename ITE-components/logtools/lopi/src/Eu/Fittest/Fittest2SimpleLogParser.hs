{-# LANGUAGE Rank2Types, FlexibleContexts, MultiParamTypeClasses #-}

module Eu.Fittest.Fittest2SimpleLogParser 
       ( convFittest2SimpleLog
       , loadSimpleLog
       , getEventList
       , collectAllEvents
       , runParser
       ) where

import Eu.Fittest.Data
import Text.ParserCombinators.UU (mapM, P, parse_h)
import Text.ParserCombinators.UU.Utils hiding (runParser)
import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import Text.ParserCombinators.UU.Derived (pList, pMany, pPacked)
import Eu.Fittest.Logging.Compression.RawlogParser
import Data.List (isSuffixOf, isPrefixOf)

import System.IO
import GHC.IO.Handle.Types
import Data.ListLike (ListLike)
import Data.Char
import System.FilePath.Posix
import qualified Data.Text.IO as T
import qualified Data.Text as Text


type RawLogAST = RawLogEntry UTCTimeStamp1970 String [String]

type Parser b a = (Show b, IsLocationUpdatedBy loc b, ListLike state b) => P (Str b state loc) a

instance IsLocationUpdatedBy Int Parameter where
  advance loc str = 5

toList :: RawLogAST -> [String]
toList (Section ts attr rlas) = concat $ map toList rlas
toList (Par par) = par

pSimpleLog :: Parser Parameter Log
pSimpleLog = pList pSimpleLogEntry

pSimpleLogEntry :: Parser Parameter LogEntry
pSimpleLogEntry = (\evt st -> LogEntry st evt) 
                  <$> pSimpleEvent 
                  <*> pSimpleState

pSimpleState :: Parser Parameter State
pSimpleState = 
  (\_ vars -> State vars)
  <$> pSatisfyDef isID
  <*> pMany (((\_ _ args -> foldr (++) "" $ map getVal  args)
              <$>pSatisfyDef isRef 
              <*> pSatisfyDef isID 
              <*> pMany (pSatisfyDef isElem)
             ) 
              <<|>
             (\arg -> getVal arg) <$> pSatisfyDef isParameter
            )

pSimpleEvent :: Parser Parameter CEvent
pSimpleEvent = CEvent <$> pAEvent <*> pCEArgs

pAEvent :: Parser Parameter AEvent
pAEvent = 
  (\_ (Param _ target _) (Param _ typ _) -> typ ++ target ) 
  <$> pSatisfyDef isID 
  <*> pSatisfyDef isTargetId 
  <*> pSatisfyDef isType 
          
pCEArgs :: Parser Parameter CEArgs
pCEArgs = 
  (\_ _ _ args -> map getVal args)
  <$> pMany (pSatisfyDef isParameter) 
  <*> pSatisfyDef isArgs
  <*> pSatisfyDef isID
  <*> pMany (pSatisfyDef isElem)

-- | An intermediate datatype that represents serialized (param, value, type) triplies

type PVar  = String
type PVal  = String
type PType = String
data Parameter = Param { getVar  ::  PVar
                       , getVal  ::  PVal
                       , getType :: PType
                       } 
                 deriving (Show)

isTargetId :: Parameter -> Bool
isTargetId (Param var _ _) | var == "targetID" = True
                           | otherwise         = False
                                                 
isType :: Parameter -> Bool
isType (Param var _ _) | var == "type" = True
                       | otherwise     = False
                                         
isArgs :: Parameter -> Bool
isArgs (Param var _ _) | var == "args" = True
                       | otherwise     = False
                                         
isID :: Parameter -> Bool
isID (Param var _ _) | var == "I" = True
                     | otherwise  = False
                                    
isElem :: Parameter -> Bool
isElem (Param var _ _) | var == "elem" = True
                       | otherwise     = False

isRef :: Parameter -> Bool
isRef (Param _ val _) | val == ">" = True
                      | otherwise  = False

pManyTill :: Parser c a -> Parser c b -> Parser c [a]
pManyTill p end = [] <$ end 
                  <<|> 
                  (:) <$> p <*> pManyTill p end

pSatisfyDef :: (Parameter -> Bool) -> Parser Parameter Parameter
pSatisfyDef pred = pSatisfy pred (Insertion "undefined parameter" (Param "" "" "") 5) 

pParameter :: Parser Char Parameter
pParameter = (\var val typ -> Param var val typ) 
             <$> pManyTill pLetterOrDigit (pSym '=')
             <*> ( ( ( pPackedString (pMany pLetterOrDigit) 
                       <|> 
                       pMany pLetterOrDigit
                     ) <* pSym ':'
                   )
                   <|>
                   (\x -> [x]) <$> pSym '>'
                 ) 
             <*> pMany pLetterOrDigit

isParameter :: Parameter -> Bool
isParameter (Param _ _ _) = True

runParser ::  (Show b, IsLocationUpdatedBy Int b) => Parser b a -> [b] -> a
runParser p = fst. parse_h ((,) <$> p <*> pEnd) . createStr (5::Int)

pLetterOrDigit :: Parser Char Char
pLetterOrDigit = pDigit <|> pLetter <|> pSym '_'

pPackedString :: Parser Char a -> Parser Char a
pPackedString p = pPacked (pToken "\"") (pToken "\"") p


-- runConversion :: FilePath -> IO Log
-- runConversion path = do log <- T.readFile path
--                         let parsed_log = generalizeLog $ pFittest2SimpleLog $ strParseRawLog log
--                         let conv_path = "c_" ++ takeFileName path
--                         writeFile conv_path (show parsed_log)
--                         return parsed_log

-- testConvert :: IO ()

loadSimpleLog :: FilePath -> IO Log
loadSimpleLog path = do log_str <- readFile $ replaceExtension path "slog"
                        return (read log_str :: Log)

patchLog :: FilePath -> IO ()
patchLog path = 
  do raw_log <- T.readFile path
     let raw_log_ast    = map flexstorePatch $ strParseRawLog raw_log
     print raw_log_ast    
  

-- | some flexstore events such as purchase(i) don't have explicit argument during logging, the argument is loged as part of the event name. the following patch handles this problem by splitting the argument from the event name and substituting it as the parameter
flexstorePatch :: RawLogAST -> RawLogAST
flexstorePatch 
  (Section ts t 
   [ evt
   , Section ss st 
     [ Par [id, ncat, nshop, shop_tot, ncomp, cat]
     , cat_cont
     , shop
     , shop_cont
     , comp
     , comp_cont
     ]
   ]) = let Section _ _ [Par cat_cont']  = cat_cont 
            Section _ _ [Par shop_cont'] = shop_cont
            Section _ _ [Par comp_cont'] = comp_cont 
            ncat' = case splitParam ncat of 
              (p,d,r) -> p ++ (show $ (length cat_cont') - 1) ++ r
            nshop' = case splitParam nshop of 
              (p,d,r) -> p ++ (show $ (length shop_cont') - 1) ++ r
            ncomp' = case splitParam ncomp of 
              (p,d,r) -> p ++ (show $ (length comp_cont') - 1) ++ r
    in Section ts t 
       [evt
       , Section ss st 
         [Par [id, ncat', nshop', shop_tot, ncomp', cat]
         , cat_cont
         , shop
         , shop_cont
         , comp
         , comp_cont]
       ]

takeDigits :: String -> (String, String)
takeDigits (s:ss) = let (ds, nds) = takeDigits ss
                    in  if isDigit s 
                        then (s:ds, nds)
                        else ([], s:ss)
                             
-- splitParam :: String -> (String, String, String)
splitParam st = let (lhs, rhs)   = break (\x -> x == '=') st
                    (dig, rest) = takeDigits $ tail rhs 
                in  (lhs ++ "=", dig, rest)


-- | this function requires some modularisation to abstract over flexstorePatch
convFittest2SimpleLog :: FilePath -> IO ()
convFittest2SimpleLog path = 
  do raw_log <- T.readFile path
     writeFile npath $ show $ pureConversion $ Text.unpack raw_log
  where
      pureConversion log = let raw_log        = Text.pack log
                               raw_log_ast    = map flexstorePatch $ strParseRawLog raw_log -- strParseRawLog raw_log
                               raw_log_list   = map toList raw_log_ast
                               log_param_list = map (map (runParser pParameter)) raw_log_list 
                               simple_log     = map (runParser pSimpleLogEntry) log_param_list
                           in  generalizeLog simple_log
      npath              = replaceExtension path "slog"

      
splitLogs :: String -> [String]
splitLogs log = case span (/='\n') log of
      (ls,x1:x2:lss) -> if x2 == '\n'
                        then (ls:(splitLogs lss))
                        else let (hd:tl) = splitLogs lss
                             in  ((ls ++ [x1] ++ [x2] ++ hd):tl)
      (ls, rs)       -> [ls ++ rs]



-- | Some event names in log have suffixes that were added in their names  
-- | in order to pass extra parameter to the events. Generalization of events
-- | allow us to shift this parameters in to the event argument list.
generalizeLog :: Log -> Log
generalizeLog l = map (\(LogEntry st ev) -> LogEntry st (generalizeEvent ev)) l
  where
    generalizeEvent :: CEvent -> CEvent
    generalizeEvent (CEvent ename eargs) = let (ename', new_arg) = break (isDigit) ename
                                           in if ("" /= new_arg)
                                              then CEvent ename' (new_arg:eargs)
                                              else CEvent ename' eargs                                                 
-- | This is a testing function to get the list of all unique event names 
-- | contained in to the log. This list should contain all events that were 
-- | defined in test suite. 
getEventList :: Log -> [AEvent]
getEventList = foldl (\x y -> 
                       let ename = conEName $ event y
                       in if (ename `notElem` x)
                          then ename:x
                          else x
                     ) []
               

collectAllEvents :: FilePath -> IO [AEvent]
collectAllEvents path = 
  do log <-loadSimpleLog path
     let events = getEventList log
     writeFile (replaceExtension path "evt") $ show events
     return events 
                         