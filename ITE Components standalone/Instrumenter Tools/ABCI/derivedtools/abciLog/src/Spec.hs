-- | Contains the function |specTrans| which performs the
--   'customizable' transformations.

{-# LANGUAGE BangPatterns #-}
module Spec(specTrans) where

-- import TestTrans1
-- import DemoTrans
import SimpleLogInj
import ProgInfo
import ByteCode
import Options
import ForceAstEval


-- | Replace right-hand side with your instrumentation function.
theInstrFun = logInjection


-- | Calls the transformation in the specification file
specTrans :: Options -> [SymbolTables] -> SwfFile -> IO (Maybe SwfFile)
specTrans !opts !tbls
  | optInstrument opts = return . Just . forceSwf . theInstrFun opts tbls
  | otherwise          = return . const Nothing

forceSwf :: SwfFile -> SwfFile
forceSwf !swf = seq (forceAstEval swf) swf
