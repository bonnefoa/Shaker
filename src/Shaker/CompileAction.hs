module Shaker.CompileAction
 where

import GHC
import Control.Exception
import System
import Exception
import Control.Monad.Trans
import Control.Monad.Reader
import Outputable
import DynFlags 
import GHC.Paths
import Shaker.Io
import Shaker.Type
import Shaker.Config

-- runCompileProject :: ReaderT ShakerConfig IO[String]
runCompileProject =  listProjectFiles >>= \a -> runReaderT (runCompile a) defaultConfig

--runCompile :: [String] -> IO [String]
runCompile targetFiles = do
        ga <-  ask 
        defaultErrorHandler defaultDynFlags $ do
	runGhc (Just libdir) $ do
	dflags <- getSessionDynFlags
	setSessionDynFlags dflags {
          importPaths = ["src/","testsuite/tests/"], 
          verbosity = 1, 
          objectDir = Just "target",
          hiDir = Just "target",
          packageFlags = [ExposePackage "ghc"]
        }
	target <- mapM (\a -> guessTarget a Nothing) targetFiles
	setTargets target
	load LoadAllTargets
	g <- getModuleGraph
	mapM showModule g     

