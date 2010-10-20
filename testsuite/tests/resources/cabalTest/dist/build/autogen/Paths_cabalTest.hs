module Paths_cabalTest (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

binfileListenInfoDir, libdir, datafileListenInfoDir, libexecfileListenInfoDir :: FilePath

binfileListenInfoDir     = "/home/sora/.cabal/bin"
libdir     = "/home/sora/.cabal/lib/cabalTest-0.0.1/ghc-6.12.3"
datafileListenInfoDir    = "/home/sora/.cabal/share/cabalTest-0.0.1"
libexecfileListenInfoDir = "/home/sora/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "cabalTest_binfileListenInfoDir") (\_ -> return binfileListenInfoDir)
getLibDir = catch (getEnv "cabalTest_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "cabalTest_datafileListenInfoDir") (\_ -> return datafileListenInfoDir)
getLibexecDir = catch (getEnv "cabalTest_libexecfileListenInfoDir") (\_ -> return libexecfileListenInfoDir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  fileListenInfoDir <- getDataDir
  return (fileListenInfoDir ++ "/" ++ name)
