{-

  Utility to specify log-points.
  
  
-}

module LogPointsLib(
    LogPointFilter(..),
    LogInjType(..),
    LogPointSpec(..),
    parseLogPointSpecs,
    logFunFilter,
    logBlockFilter,
    logFieldFilter
    )

 where

import Data.List
import Data.Char
import Data.Maybe

data LogPointFilter =
       ALL
     | FNis !String
     | FNhasPrefix !String
     | FNhasSuffix  !String
     | FNcontains  !String
     | CNis !String
     | CNhasPrefix !String
     | CNhasSuffix  !String
     | CNcontains  !String
     deriving (Show,Read)
     
data LogInjType =
      LogFunc            -- match on AS functions/methods
    | LogBlock           -- match on blocks
    | LogField String [String]  -- match on constructors, used to inject field logging
                                -- syntax: LogField idfragment fields
    -- | LogCall
    -- | LogExcHandler
    -- | LogLoop
    deriving (Show,Read,Eq)
    
isLogField (LogField _ _) = True
isLogField _            = False

getLogFieldInfo (LogField idfragment fs) = (idfragment,fs)
  
data LogPointSpec = [LogPointFilter] :>->: LogInjType
    deriving (Show,Read)

-- for parsing log-point specifications
--    
parseLogPointSpecs :: String -> [LogPointSpec]
parseLogPointSpecs input =  map read . filter (any (not . isSpace)) . lines $ input

ok__  _  _   ALL            =  True
ok__  fn _  (FNis s)        =  s == fn
ok__  fn _  (FNhasPrefix s) =  s `isPrefixOf` fn
ok__  fn _  (FNhasSuffix s) =  s `isSuffixOf` fn
ok__  fn _  (FNcontains s)  =  s `isInfixOf`  fn
ok__  _ cn  (CNis s)        =  s == cn
ok__  _ cn  (CNhasPrefix s) =  s `isPrefixOf` cn
ok__  _ cn  (CNhasSuffix s) =  s `isSuffixOf` cn
ok__  _ cn  (CNcontains  s) =  s `isInfixOf`  cn

logPointFilter :: LogInjType -> [LogPointSpec] -> String -> String -> Bool
logPointFilter injType [] fn cn = False 
logPointFilter injType lpss fn cn 
   = 
   or 
   $! 
   [ clauseOk fn cn $! clause | (clause :>->: injtype_) <- lpss, injtype_ == injType ]

clauseOk fn cn clause = all (ok__ fn cn) clause
   
-- interpert log point specifications to decide whether or not to log on a candidate point
-- 
logFunFilter   = logPointFilter LogFunc
logBlockFilter = logPointFilter LogBlock

logFieldFilter lpss fn cn = take 1 matches
   where
   matches = [ getLogFieldInfo injType 
                        | (clause :>->: injType) <- lpss,
                           isLogField injType && clauseOk fn cn clause
             ]
              

ex1 = "[FNhasPrefix \"foo\", ALL] :>->: LogBlock"
ex2 = "[CNhasPrefix \"foo\", FNis \"\"] :>->: LogField \"btn0\" [\"x\",\"y\"]"



