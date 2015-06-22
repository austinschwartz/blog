module Paths_austin (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/aschwartz/.cabal/bin"
libdir     = "/Users/aschwartz/.cabal/lib/x86_64-osx-ghc-7.10.1/austi_4SIsruhHFKyF7BV19EXdzk"
datadir    = "/Users/aschwartz/.cabal/share/x86_64-osx-ghc-7.10.1/austin-0.1.0.0"
libexecdir = "/Users/aschwartz/.cabal/libexec"
sysconfdir = "/Users/aschwartz/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "austin_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "austin_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "austin_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "austin_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "austin_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
