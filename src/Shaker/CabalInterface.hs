module Shaker.CabalInterface 
 where

import Control.Monad.Reader
import Control.Arrow
import Distribution.PackageDescription
import Distribution.Simple.Build
import Distribution.Simple.GHC(ghcOptions)
import Distribution.Simple.LocalBuildInfo 
import Distribution.Simple.PreProcess
import Distribution.Verbosity
import Shaker.Type
import System.FilePath

generateAutogenFiles :: LocalBuildInfo -> IO ()
generateAutogenFiles lbi = writeAutogenFiles normal (localPkgDescr lbi) lbi

applyPreprocessSources :: Shaker IO ()
applyPreprocessSources = do
  lbi <- asks shakerLocalBuildInfo 
  let pkgDescription = localPkgDescr lbi
  lift $ preprocessSources pkgDescription lbi False normal knownSuffixHandlers 

getPreprocessorDirectory :: LocalBuildInfo -> Executable -> FilePath
getPreprocessorDirectory lbi Executable {exeName = exeName'}= 
  buildDir lbi </> exeName' </> exeName' ++ "-tmp"

getCompileFlagsForExecutable :: LocalBuildInfo -> Executable -> ComponentLocalBuildInfo -> [String]
getCompileFlagsForExecutable lbi executable componentLocalBuildInfo =
  ghcOptions lbi (buildInfo executable) componentLocalBuildInfo defaultDistDir ++ preprocessLocation
    where preprocessLocation = ["-i" ++ getPreprocessorDirectory lbi executable]

getCompileFlagsForLibrary :: LocalBuildInfo -> Library -> ComponentLocalBuildInfo -> [String]
getCompileFlagsForLibrary lbi lib componentLocalBuildInfo = 
  preprocessLocation : ghcOptions lbi (libBuildInfo lib) componentLocalBuildInfo defaultDistDir 
    where preprocessLocation = "-i" ++getPreprocessorDirectory lbi (localPkgDescr >>> executables >>> head $ lbi)

