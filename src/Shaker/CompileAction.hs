module Shaker.CompileAction
 where

import GHC
import Outputable
import DynFlags 
import GHC.Paths
import Shaker.Io

runCompileProject = listProjectFiles >>= runCompile 

runCompile :: [String] -> IO [String]
runCompile targetFiles = defaultErrorHandler defaultDynFlags $ do
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


