module Shaker.CabalInterface 
 where

import Control.Monad.Reader
import Distribution.Simple.Build
import Distribution.Simple.LocalBuildInfo 
import Distribution.Simple.PreProcess
import Distribution.Verbosity
import Shaker.Type

generateAutogenFiles :: LocalBuildInfo -> IO ()
generateAutogenFiles lbi = writeAutogenFiles normal (localPkgDescr lbi) lbi

applyPreprocessSources :: Shaker IO ()
applyPreprocessSources = do
  lbi <- asks shakerLocalBuildInfo 
  let pkgDescription = localPkgDescr lbi
  lift $ preprocessSources pkgDescription lbi False normal knownSuffixHandlers 

