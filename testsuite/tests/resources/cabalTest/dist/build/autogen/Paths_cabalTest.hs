module Paths_cabalTest (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/sora/.cabal/bin"
libdir     = "/home/sora/.cabal/lib/cabalTest-0.0.1/ghc-6.12.3"
datadir    = "/home/sora/.cabal/share/cabalTest-0.0.1"
libexecdir = "/home/sora/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "cabalTest_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "cabalTest_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "cabalTest_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "cabalTest_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
