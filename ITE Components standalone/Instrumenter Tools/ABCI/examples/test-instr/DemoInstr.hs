-- | Example of an Instrumentation configuration file
--
-- It imports the generate symbol tables of DemoSym

module Main where

import Instr
import InstrBaseLib
import DemoSym
import PrettySpec


instrs :: [Instr ()]
instrs =
  [ do mBody <- onPrevious $ matchPropEnter k'gcd_Gcd'recGcd
       assertAll [ firstParam mBody  .>=. (2 :: Int)
                 , secondParam mBody .>=. (2 :: Int)
                 ]
       mCall <- matchPropCall k'gcd_Gcd'recGcd
       assert (firstParam mCall .>=. (1 :: Int))
       callProp (staticProp k'gcd_LogUtils'logFunCall) (funName $ matchInfo mCall)
       return ()
  , do mBody <- onPrevious $ matchPropEnter k'gcd_Gcd'whileGcd
       mBlock <- matchBlockEntry
       assert $ blockCycl $ matchInfo mBlock
       callProp (staticProp k'gcd_LogUtils'logLoopEnter) ()
       return ()
  ]

main :: IO ()
main = do
  putStrLn "low-level instrumentation instructions"
  putStrLn $ show $ ppSpec compiled
  where
    myInstr  = foldr (<#>) (fail "initial") instrs
    compiled = runInstr myInstr
