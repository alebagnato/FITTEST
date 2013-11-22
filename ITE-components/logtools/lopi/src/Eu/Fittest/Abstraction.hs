module Eu.Fittest.Abstraction where

import Eu.Fittest.Data

type AbsFun = State -> State

projState :: [Var] -> AbsFun
projState pvs (State vs)  = State $ filter (`elem` pvs) vs

idAbs :: AbsFun
idAbs = id

readAF :: String -> AbsFun
readAF str = case str of
  "idAbs"     -> idAbs
  "projState" -> projState [] -- for now it only handles empty projection

runAbsFun :: AbsFun -> Log -> Log
runAbsFun abs log = map absLogEntry log
  where
    absLogEntry le = let state' = abs $ state le
                     in  le{state=state'}
