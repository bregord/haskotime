module Paths_520Assignment1 (
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

bindir     = "/home/brendan/.cabal/bin"
libdir     = "/home/brendan/.cabal/lib/i386-linux-ghc-7.10.2/520Assignment1-0.1.0.0-0uuUurfuUMP5utobyuA1re"
datadir    = "/home/brendan/.cabal/share/i386-linux-ghc-7.10.2/520Assignment1-0.1.0.0"
libexecdir = "/home/brendan/.cabal/libexec"
sysconfdir = "/home/brendan/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "520Assignment1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "520Assignment1_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "520Assignment1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "520Assignment1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "520Assignment1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
