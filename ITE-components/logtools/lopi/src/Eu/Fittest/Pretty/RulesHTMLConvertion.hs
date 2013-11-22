module  Eu.Fittest.Pretty.RulesHTMLConversion where

import Eu.Fittest.Oracles.Ast.OrcsDataType
import Eu.Fittest.Oracles.Pretty.HTMLPrettyPrinter
import Eu.Fittest.WitnessSetParser
import Eu.Fittest.Data
import Eu.Fittest.Pretty.Events
import Text.PrettyPrint.HughesPJ (render)
import Data.List (groupBy)
import Data.Function (on)
import Debug.Trace

witnesses2OracleSute :: Witnesses -> OracleSuite
witnesses2OracleSute ws =
  let wgroups   = groupBy (\ws1 ws2 -> ((==) `on` getWitnessType) ws1 ws2) ws
      subgroups = map witness2subgroup wgroups
  in [("Group", subgroups)] 

witness2subgroup :: Witnesses -> OracleSubGroup    
witness2subgroup ws =
  let evType  = ET_AppEvent "test" (getWitnessType $ head ws)
      judgeWit (WitnessC _ _ (p,n)) | n > 0  = Violated
                                    | p == 0 = Undecided
                                    | p > 0  = Pass
      showWitness w@(WitnessC t rr (p,n)) =
        (Pattern $ O_Pattern $ render $ ppRewRulPat rr, Just p, Just (judgeWit w, "negative witnesses: " ++ show n))
      oracles = map showWitness ws
  in  (evType, oracles)


htmlPrettyWitnesses :: FilePath -> FilePath -> IO ()
htmlPrettyWitnesses ifp ofp = do
  ws <- readFile ifp >>= return . getWitnesses
  let osuite = witnesses2OracleSute ws
  orcsSuite2Html ofp osuite
