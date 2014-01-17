module Paths_abciLog (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/alex/.cabal/bin"
libdir     = "/home/alex/.cabal/lib/abciLog-1.0/ghc-7.6.1"
datadir    = "/home/alex/.cabal/share/abciLog-1.0"
libexecdir = "/home/alex/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "abciLog_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "abciLog_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "abciLog_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "abciLog_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
