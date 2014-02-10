module FlexstoreSpec where

import Eu.Fittest.RPInferencer (countPats, returnNegWit)
import Eu.Fittest.Data
import Data.List (groupBy, nub, (\\), )
import qualified PrettyEvents as PPEvt
import qualified PrettyHOL as PPHol
import Text.PrettyPrint.HughesPJ (render)
import Data.Maybe
import Eu.Fittest.Fittest2SimpleLogParser 
import Eu.Fittest.Filter (filterWitnesses, Filter (..), defConfLevelFun)
import System.FilePath.Posix
import Debug.Trace
-- import Rewriting
import EventRewriting

-- | This module lists the events possible in the flexstore application and
-- | together with rewrite-rile patterns that we are going to check for them. 

exp_path = "/home/alex/PROJECTS/FITTEST/UUexamples/flexstore/v1.2.2.clean/exp/"

convLog2HOL :: FilePath -> Log -> IO ()
convLog2HOL path log = writeFile path (render $ PPHol.ppLog log)

convLog2HolByParts :: FilePath -> Log -> Int -> IO ()
convLog2HolByParts path log n = convLog2HolByParts' path log n 0
  where
    convLog2HolByParts' :: FilePath -> Log -> Int -> Int -> IO ()
    convLog2HolByParts' path [] n i = return () 
    convLog2HolByParts' path l n i = 
      do let (log_seg, log_new) = splitAt n l
             new_bname = trace (show i) (takeBaseName path ++ show i)   
         writeFile (replaceBaseName path new_bname) (render $ PPHol.ppLog log_seg)
         convLog2HolByParts' path log_new n (i+1)

convRRules2HOL :: Show t => FilePath -> [Witness t] -> IO ()
convRRules2HOL path ws = writeFile path (render $ PPHol.ppWitnesses ws)

convAEvents2HOL :: FilePath -> [AEvent] -> IO ()
convAEvents2HOL path aevts = writeFile path (render $ PPHol.ppAEvents aevts)

rewRulePatInf :: Log -> [AEvent] -> [Witness Com]
rewRulePatInf log aevts = countPats (com aevts) log

rewRulePatInf' :: Log -> [AEvent] -> ([AEvent] -> [RRPattern RewRulPat t]) -> [Witness t]
rewRulePatInf' log aevts pat = countPats (pat aevts) log

runConversion :: FilePath -> IO ()
runConversion path = 
  do convFittest2SimpleLog path
     flexstoreEvents <- collectAllEvents path
     log             <- loadSimpleLog path
     let abs_log      = runAbsFun log (projAbsFun [0,1]) 
     -- let abs_log      = log
         wSet         = rewRulePatInf abs_log flexstoreEvents
         wSetF        = filterWitnesses wSet (FilterZero)
     writeFile "witSet.out" (render $ PPEvt.ppWSet wSetF)
     -- convLog2HolByParts "../hol/flexstore/FlexstoreLog.mx" abs_log 1000
     convLog2HOL     "../hol/flexstore/FlexstoreLog.mx" abs_log
     convRRules2HOL  "../hol/flexstore/Rules.mx"        wSetF
     convAEvents2HOL "../hol/flexstore/Events.mx"       flexstoreEvents
     
returnWitnesses :: (Show t) => Log -> Int -> Filter -> [AEvent] -> ([AEvent] -> [RRPattern RewRulPat t]) -> [RewRulPat] 
returnWitnesses log n filt aevs evGen = 
  let log' = take n log
      ws   = rewRulePatInf' log' aevs evGen
      wsF  = filterWitnesses ws filt
  in map getRewRulPat wsF

countAllFlxPats' :: Log -> [AEvent] -> Filter -> Int -> Template 
countAllFlxPats' log flxes fil n =
  let skpRs          = returnWitnesses log n fil flxes skip
      skpEs          = map (symEName . head . getLhsRewRulPat) skpRs
      nonSkpEs       = flxes \\ skpEs
      nonSkpAndSldEs = nonSkpEs \\ ["changepriceSlider"]
      zrRs           = returnWitnesses log n fil nonSkpEs zero 
      -- zrRs'          = zrRs ++ mkZrFrSkp flxes skpEs
      idpRs          = returnWitnesses log n fil nonSkpEs idemp 
      -- idpRs'         = idpRs ++ mkIdpFrSkp skpEs
      ignRs          = returnWitnesses log n fil nonSkpAndSldEs ignore 
      -- ignRs'         = ignRs ++ mkIgnFrSkp skpEs
      comRs          = returnWitnesses log n fil nonSkpEs com 
      -- comRs'         = comRs ++ mkComFrSkp flxes skpEs
  in [skpRs, zrRs, idpRs, ignRs, comRs]

countAllFlxPats :: Log -> [AEvent] -> Filter -> Int -> Template 
countAllFlxPats log flxes fil n =
  let skpRs          = returnWitnesses log n fil flxes skip
      skpEs          = map (symEName . head . getLhsRewRulPat) skpRs
      nonSkpEs       = flxes \\ skpEs
      nonSkpAndSldEs = nonSkpEs \\ ["changepriceSlider"]
      zrRs           = returnWitnesses log n fil nonSkpEs zero 
      zrRs'          = zrRs ++ mkZrFrSkp flxes skpEs
      idpRs          = returnWitnesses log n fil nonSkpEs idemp 
      idpRs'         = idpRs ++ mkIdpFrSkp skpEs
      ignRs          = returnWitnesses log n fil nonSkpAndSldEs ignore 
      ignRs'         = ignRs ++ mkIgnFrSkp skpEs
      comRs          = returnWitnesses log n fil nonSkpEs com 
      comRs'         = comRs ++ mkComFrSkp flxes skpEs
  in [skpRs, zrRs', idpRs', ignRs', comRs']

testReduction :: FilePath -> FilePath ->  IO ()
testReduction rpath lpath = 
  do convFittest2SimpleLog lpath
     flstEs        <- collectAllEvents lpath
     llog           <- loadSimpleLog lpath
     print $ length llog
     let alog1      = runAbsFun llog id
         templ1     = countAllFlxPats alog1 flstEs (FilterFun defConfLevelFun) 10000
         alog1Evs   = map event alog1
         realLogConf = returnAllMatches alog1 flstEs (FilterFun defConfLevelFun) templ1 [10000]  
     convFittest2SimpleLog rpath 
     rlog           <- loadSimpleLog rpath
     let evt_log = map event rlog
     -- print $ templ1
     -- print $ rlog
     -- print $ length $ runRewrite (take 100 evt_log) (concat templ1)
     print $ map (\rs -> length $ runRewrite (take 5000 alog1Evs)  (concat rs)) realLogConf
     
zeroRedTest :: IO ()     
zeroRedTest = 
  do let rules = [[SEvent {symEName = "e", symEArgs = SEArgs {symEvtArgs = "p"}},SEvent {symEName = "d", symEArgs = SEArgs {symEvtArgs = "q"}}] :~: [SEvent {symEName = "d", symEArgs = SEArgs {symEvtArgs = "q"}}],[SEvent {symEName = "d", symEArgs = SEArgs {symEvtArgs = "p"}},SEvent {symEName = "e", symEArgs = SEArgs {symEvtArgs = "q"}}] :~: [SEvent {symEName = "e", symEArgs = SEArgs {symEvtArgs = "q"}},SEvent {symEName = "d", symEArgs = SEArgs {symEvtArgs = "p"}}],[SEvent {symEName = "c", symEArgs = SEArgs {symEvtArgs = "p"}},SEvent {symEName = "d", symEArgs = SEArgs {symEvtArgs = "q"}}] :~: [SEvent {symEName = "d", symEArgs = SEArgs {symEvtArgs = "q"}},SEvent {symEName = "c", symEArgs = SEArgs {symEvtArgs = "p"}}],[SEvent {symEName = "c", symEArgs = SEArgs {symEvtArgs = "p"}},SEvent {symEName = "f", symEArgs = SEArgs {symEvtArgs = "q"}}] :~: [SEvent {symEName = "f", symEArgs = SEArgs {symEvtArgs = "q"}}]]
         log = [LogEntry {state = State ["1"], event = CEvent {conEName = "c", conEArgs = ["0"]}},LogEntry {state = State ["1"], event = CEvent {conEName = "d", conEArgs = ["0"]}},LogEntry {state = State ["1"], event = CEvent {conEName = "e", conEArgs = ["0"]}},LogEntry {state = State ["1"], event = CEvent {conEName = "f", conEArgs = ["0"]}}]
     print $ runRewrite (map event log) rules
     
rules = [[SEvent {symEName = "e", symEArgs = SEArgs {symEvtArgs = "p"}},SEvent {symEName = "d", symEArgs = SEArgs {symEvtArgs = "q"}}] :~: [SEvent {symEName = "d", symEArgs = SEArgs {symEvtArgs = "q"}}]]

crule = [SEvent {symEName = "e", symEArgs = SEArgs {symEvtArgs = "p"}},SEvent {symEName = "f", symEArgs = SEArgs {symEvtArgs = "q"}}] :~: [SEvent {symEName = "f", symEArgs = SEArgs {symEvtArgs = "q"}},SEvent {symEName = "e", symEArgs = SEArgs {symEvtArgs = "p"}}]

zrule = [SEvent {symEName = "e", symEArgs = SEArgs {symEvtArgs = "p"}},SEvent {symEName = "d", symEArgs = SEArgs {symEvtArgs = "q"}}] :~: [SEvent {symEName = "d", symEArgs = SEArgs {symEvtArgs = "q"}}]       
  
llog = [LogEntry {state = State ["1"], event = CEvent {conEName = "e", conEArgs = ["0"]}},LogEntry {state = State ["1"], event = CEvent {conEName = "f", conEArgs = ["0"]}},LogEntry {state = State ["1"], event = CEvent {conEName = "d", conEArgs = ["0"]}}]
  
retTblRes :: FilePath -> IO ()
retTblRes path =
  do convFittest2SimpleLog path
     flstEs        <- collectAllEvents path
     log           <- loadSimpleLog path
     let alog1             = runAbsFun log id 
         templ1            = countAllFlxPats' alog1 flstEs (FilterFun defConfLevelFun) 11000
         realLogConf       = countAllMatchs' alog1 flstEs (FilterFun defConfLevelFun) templ1 [5000, 2500, 1000, 500, 100] 
         realLogFilterZero = countAllMatchs' alog1 flstEs (FilterZero) templ1 [5000, 2500, 1000, 500, 100]
         alog2             = runAbsFun log (projAbsFun [0,1]) 
         templ2            = countAllFlxPats' alog2 flstEs (FilterFun defConfLevelFun) 11000
         absLogConf        = countAllMatchs' alog2 flstEs (FilterFun defConfLevelFun) templ2 [5000, 2500, 1000, 500, 100] 
         absLogFilterZero  = countAllMatchs' alog2 flstEs (FilterZero) templ2 [5000, 2500, 1000, 500, 100]
     writeFile "final.tbl" "--\nOriginal Log. Witnesses are filtered by conf--\n"
     appendFile "final.tbl" (show realLogConf)
     appendFile "final.tbl" "--\nOriginal Log. Witnesses aren't filtered by conf--\n"
     appendFile "final.tbl" (show realLogFilterZero)
     appendFile "final.tbl" "--\nAbstract Log. Witnesses are filtered by conf--\n"
     appendFile "final.tbl" (show absLogConf)
     appendFile "final.tbl" "--\nAbstract Log. Witnesses aren't filtered by conf--\n"
     appendFile "final.tbl" (show absLogFilterZero)
     
testInference :: FilePath -> IO ()
testInference path = 
  do convFittest2SimpleLog path
     flstEs        <- collectAllEvents path
     log           <- loadSimpleLog path
     let alog1      = runAbsFun log id
     let templ1     = countAllFlxPats alog1 flstEs (FilterFun defConfLevelFun) 1000
     let realLogConf = head $ returnAllMatches alog1 flstEs (FilterFun defConfLevelFun) templ1 [1000] 
     let alog2      = runAbsFun log (projAbsFun [0,1]) 
     let templ2     = countAllFlxPats alog2 flstEs (FilterFun defConfLevelFun) 1000
     let absLogConf = head $ returnAllMatches alog2 flstEs (FilterFun defConfLevelFun) templ2 [1000] 
     --print realLogConf
     print $ zipWith (\\) absLogConf realLogConf
     
retRewTblRes :: FilePath -> IO ()
retRewTblRes path =
  do convFittest2SimpleLog path
     flstEs        <- collectAllEvents path
     log           <- loadSimpleLog path
     let alog1      = runAbsFun log id 
     let alog1Evs   = map event alog1
     let templ1     = countAllFlxPats' alog1 flstEs (FilterFun defConfLevelFun) 11000
     writeFile "final.tbl" "-- Valid rules in original log--\n"
     -- appendFile "final.tbl" $ show templ1
     let realLogConf = returnAllMatches' alog1 flstEs (FilterFun defConfLevelFun) templ1 [5000, 2500, 1000, 500, 100] 
     -- appendFile "final.tbl" "\n-- Valid rules in original log filtered with conf 5000....--\n"
     -- appendFile "final.tbl" $ show realLogConf
     let  realLogFilterZero = returnAllMatches' alog1 flstEs (FilterZero) templ1 [5000, 2500, 1000, 500, 100]
     -- appendFile "final.tbl" "\n-- Valid rules in original log not filtered....--\n"
     -- appendFile "final.tbl" $ show realLogFilterZero
     let alog2      = runAbsFun log (projAbsFun [0,1]) 
     let alog2Evs   = map event alog2
     let templ2     = countAllFlxPats' alog2 flstEs (FilterFun defConfLevelFun) 11000
     -- appendFile "final.tbl" "\n-- Valid rules in abstract log--\n"
     -- appendFile "final.tbl" $ show templ2
     let absLogConf = returnAllMatches' alog2 flstEs (FilterFun defConfLevelFun) templ2 [5000, 2500, 1000, 500, 100] 
     -- appendFile "final.tbl" "\n-- Valid rules in abstracted log filtered with conf 5000....--\n"
     -- appendFile "final.tbl" $ show absLogConf
     let absLogFilterZero = returnAllMatches' alog2 flstEs (FilterZero) templ2 [5000, 2500, 1000, 500, 100]
     -- appendFile "final.tbl" "\n-- Valid rules in abstracted log not filtered....--\n"
     -- appendFile "final.tbl" $ show absLogFilterZero    
     appendFile "final.tbl" "--\nOriginal Log. Witnesses are filtered by conf--\n"
     appendFile "final.tbl" $ show $ map (\rs -> length $ runRewrite (take 5000 alog1Evs)  (concat rs)) (templ1:realLogConf)
     appendFile "final.tbl" "--\nOriginal Log. Witnesses aren't filtered by conf--\n"
     appendFile "final.tbl" $ show $ map (\rs -> length $ runRewrite (take 5000 alog1Evs)  (concat rs)) (templ1:realLogFilterZero)
     appendFile "final.tbl" "--\nAbstract Log. Witnesses are filtered by conf--\n"
     appendFile "final.tbl" $ show $ map (\rs -> length $ runRewrite (take 5000 alog2Evs)  (concat rs)) (templ2:absLogConf)
     appendFile "final.tbl" "--\nAbstract Log. Witnesses aren't filtered by conf--\n"
     appendFile "final.tbl" $ show $ map (\rs -> length $ runRewrite (take 5000 alog2Evs)  (concat rs)) (templ2:absLogFilterZero)

countAllMatchs :: Log -> [AEvent] -> Filter -> Template -> [Int] -> [[(Int, Int, Int)]]     
countAllMatchs log evs fil templ ns = 
  let res     = map  (countAllFlxPats log evs fil) ns
      matches = map (map sumRes . zipWith  matchWthTempl templ) res 
  in matches

countAllMatchs' :: Log -> [AEvent] -> Filter -> Template -> [Int] -> [[(Int, Int, Int)]]     
countAllMatchs' log evs fil templ ns = 
  let res     = map  (countAllFlxPats' log evs fil) ns
      matches = map (map sumRes . zipWith  matchWthTempl templ) res 
  in matches

returnAllMatches :: Log -> [AEvent] -> Filter -> Template -> [Int] -> [[[RewRulPat]]]     
returnAllMatches log evs fil templ ns = 
  let res     = map  (countAllFlxPats log evs fil) ns
      matches = map (map getMatchWit . zipWith  matchWthTempl templ) res 
  in matches
  
returnAllMatches' :: Log -> [AEvent] -> Filter -> Template -> [Int] -> [[[RewRulPat]]]     
returnAllMatches' log evs fil templ ns = 
  let res     = map  (countAllFlxPats' log evs fil) ns
      matches = map (map getMatchWit . zipWith  matchWthTempl templ) res 
  in matches     
     
getStats :: FilePath -> IO ()
getStats path = 
  do convFittest2SimpleLog path
     flexstoreEvents   <- collectAllEvents path
     log               <- loadSimpleLog path
     let --abs_log       = runAbsFun (projAbsFun [0,1]) log     
         abs_log       = runAbsFun log id 
         wsSkip        = rewRulePatInf' abs_log flexstoreEvents skip
         wsSkipF       = filterWitnesses wsSkip (FilterFun defConfLevelFun)
         skipEvents    = map (symEName . head . getLhsRewRulPat . getRewRulPat) wsSkipF
         nonSkipEvents = (flexstoreEvents \\ skipEvents)
         wsZero        = rewRulePatInf' abs_log nonSkipEvents zero
         wsZeroF       = filterWitnesses wsZero (FilterFun defConfLevelFun)
         wsIdemp       = rewRulePatInf' abs_log nonSkipEvents idemp
         wsIdempF      = filterWitnesses wsIdemp (FilterFun defConfLevelFun)
         wsIgnore      = rewRulePatInf' abs_log (nonSkipEvents \\ ["changepriceSlider"])  ignore 
         wsIgnoreF     = filterWitnesses wsIgnore (FilterFun defConfLevelFun)
         wsCom         = rewRulePatInf' abs_log nonSkipEvents com
         wsComF        = filterWitnesses wsCom (FilterFun defConfLevelFun)
     print $ show nonSkipEvents
     writeFile "patterns.res" "-----------SKIP---------\n"
     appendFile "patterns.res" (render $ PPEvt.ppWSet wsSkipF)
     appendFile "patterns.res" "\n-----------ZERO---------\n"
     appendFile "patterns.res" (render $ PPEvt.ppWSet wsZeroF)
     appendFile "patterns.res" "\n-----------IDEMP---------\n"
     appendFile "patterns.res" (render $ PPEvt.ppWSet wsIdempF)
     appendFile "patterns.res" "\n-----------IGNORE---------\n"
     appendFile "patterns.res" (render $ PPEvt.ppWSet wsIgnoreF)
     appendFile "patterns.res" "\n-----------COM---------\n"
     appendFile "patterns.res" (render $ PPEvt.ppWSet wsComF)
     
         
-- mkWSetFlexstoreTest path = 
--   do convFittest2SimpleLog path
--      flexstoreEvents <- collectAllEvents path
--      log             <- loadSimpleLog path
--      --let abs_log      = log
--      let abs_log      = stateLogAbs [0,1] log
--      putStrLn $ show $ length abs_log
--      let ws           = map (\p -> countPat' p abs_log) $ com flexstoreEvents
--      ws'             <- filterWitnesses ws (DoesNotFilter)
--      writeFile "witSet.out" (render $ PPEvt.ppWSet ws')
--      appendFile "witSet.out" "\n-----------------\n"
--      appendFile "witSet.out" (render $ PPHol.ppAEvents flexstoreEvents)
     -- appendFile "witSet.out" (render $ PPEvt.ppCEvents $ map event  log)
    -- writeFile ("hol_" ++ "flexstore500withouttab_1.mx") (render $ PPHol.ppLog log)
    -- let ws = map (\p -> countPat' p log) $ com flexstoreEvents
     -- ws' <- filterWitnesses ws (FilterZero)
     -- writeFile "witSet.out" (render $ PPEvt.ppWSet ws')
     -- appendFile "witSet.out" "\n-----------------\n"
     -- appendFile "witSet.out" (render $ PPHol.ppLog log)
     -- return ()
     
-- debugWSet path = 
--   do log <- loadSimpleLog path
--      --let abs_log = stateLogAbs [0,1] log
--      let abs_log = log
--          ws = map (\p -> returnNegWit p abs_log) $ skip ["clickview_cart"]
--      mapM (putStrLn . show) ws
 
