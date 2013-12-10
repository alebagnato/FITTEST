-- | Commandline interface to the TestIt module.
module Main(main) where

import System.IO
import TestIt
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [source]      -> runTest source (source ++ ".txt")
    [source,dest] -> runTest source dest
    _             -> hPutStrLn stderr "tester: requires <source> and optionally <dest> parameter"

runTest :: FilePath -> FilePath -> IO ()
runTest = runIt Nothing
