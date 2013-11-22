module GcdSpec where

import RPInferencer
import Gcd (Event, testGcdLog, genGcdLogInFile  )
import qualified Gcd as Gcd
import Data
import Data.List (groupBy, nub)
import qualified PrettyEvents as PPEvt
import qualified PrettyHOL as PPHol
import Text.PrettyPrint.HughesPJ (render)
import Data.Maybe


-- ====================================================
--                Gcd Example
-- ====================================================

gcdEvents = ["setX", "setY", "calc", "clear", "combo"]
gcdComEvents = com gcdEvents
gcdSkipEvents = skip gcdEvents
gcdZeroEvents = zero gcdEvents
gcdIdempEvents = idemp gcdEvents
gcdIgnoreEvents = ignore gcdEvents
allGcdRRP = gcdComEvents ++ gcdSkipEvents ++ gcdZeroEvents ++ gcdIdempEvents ++ gcdIgnoreEvents

gcdLog = concat $ replicate 150 [LogEntry (State [Just 1]) (CEvent "setX" ["2"]), LogEntry (State [Just 1]) (CEvent "setY" ["4"])]
--skipTest = [SEvent {symEName = "clear", symEArgs = SEArgs {symEvtArgs = "p"}}] :~: []

mkWSetTestGcdCom n = do log <- testGcdLog n
                        -- print log
                        let log' = convert log
                        -- writeFile "../hol/GcdLog1.mx" (render $ PPHol.ppLog log')
                        -- let w = map (\p -> checkRRPat p log' 1) gcdComEvents
                        -- print log'
                        print $ subseqsOfLog 2 log'
                        let ws = map (\p -> countPat' p log') gcdComEvents
                        -- let wsG = groupWSetGcdCom ws
                        print ws
                        -- print wsG
                        -- writeFile "witSet.out" (render $ PPEvt.ppWSet ws)
                        -- appendFile "witSet.out" "\n-----------------\n"
                        -- appendFile "witSet.out" (render $ PPEvt.ppLog log')
                        return ()

readGcdLog :: IO ()
readGcdLog = do log <- readFile "gcdlog.out" 
                let log' = convert (read log :: Gcd.Log)
                let ws   = map (\p -> countPat' p log') gcdComEvents
                let res  = render $ PPEvt.ppWSet ws
                print res
                writeFile "witSet.out" res
                
toCEvent :: Event -> CEvent
toCEvent (Gcd.Event en (_,eps)) = CEvent en (map show (fromMaybe [] eps))

toState :: Gcd.GcdState -> State
toState (Gcd.State x y r) = State [x,y,r]

convert :: Gcd.Log -> Log
convert log = map (\(Gcd.LogE evt st) -> LogEntry (toState st) (toCEvent evt)) log

{-
groupTest :: [(RewRulPat,(Int,Int))]
groupTest = [([SEvent "e" (SEArgs "p")] :~: [SEvent "e" (SEArgs "p")],(1,1)),([SEvent "d" (SEArgs "p")] :~: [SEvent "e" (SEArgs "p")],(1,1)),([SEvent "e" (SEArgs "p")] :~: [SEvent "e" (SEArgs "p")],(1,1)),([SEvent "d" (SEArgs "p")] :~: [SEvent "d" (SEArgs "p")],(1,1))]

eqTest [(x1,(y1,z1)), (x2,(y2,z2))] = x1 == x2

groupWithMy :: Ord b => (a -> b) -> [a] -> [[a]]
groupWithMy f = groupBy (\x y -> f x == f y) . sortWith f
groupWSetGcdCom w = [(the pat, (sum pos, sum neg)) 
                    | (pat, (pos, neg)) <- w
                    , then group by pat]
-}