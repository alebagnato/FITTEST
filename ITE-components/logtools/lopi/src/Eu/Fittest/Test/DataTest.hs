module DataTest where

-- ====================================================
--                 Testing Data 
-- ====================================================

-- testing zero like rewrite rule pattern (RRP)
zeroPat    = [SEvent "d" (SEArgs "p"), SEvent "e" (SEArgs "q")] :~: [SEvent "e" (SEArgs "q")]
logZero1 = [LogEntry (State [Just 2]) (CEvent "d" ["4"]), LogEntry (State [Just 3]) (CEvent "e" ["1"]), LogEntry (State [Just 2]) (CEvent "d" ["4"])] 
logZero2 = [LogEntry (State [Just 3]) (CEvent "e" ["1"]), LogEntry (State [Just 2]) (CEvent "d" ["4"])]
checkSegZero = checkSegsWithPat zeroPat logZero1 logZero2

-- testing commutativity RRP
comPat = [SEvent "e" (SEArgs "p"), SEvent "d" (SEArgs "q")] :~: [SEvent "d" (SEArgs "q"), SEvent "e" (SEArgs "p")]
logCom1 = [LogEntry (State [Just 2]) (CEvent "e" ["4"]), LogEntry (State [Just 3]) (CEvent "d" ["1"])] 
logCom2 = [LogEntry (State [Just 2]) (CEvent "d" ["1"]), LogEntry (State [Just 6]) (CEvent "e" ["4"])] 
logCom3 = [LogEntry (State [Just 21]) (CEvent "d" ["1"])]
checkSegCom = checkSegsWithPat' comPat logCom1 logCom2
mkWSetCom =  mkWSet' comPat (logCom1 ++ logCom2 ++ logCom3)
countComPat = countPat comPat (logCom1 ++ logCom2 ++ logCom3)


{-
-- SQL-like list comprehension example 
testCompr = [x| x <- "hello", y <- [1..5]]

employees = [ ("Simon", "MS", 80)
            , ("Erik", "MS", 100)
            , ("Phil", "Ed", 40)
            , ("Gordon", "Ed", 45)
            , ("Paul", "Yale", 60)]

output = [ (the dept, sum salary)
         | (name, dept, salary) <- employees
         , then group by dept
         , then sortWith by (sum salary)
         ]
-}