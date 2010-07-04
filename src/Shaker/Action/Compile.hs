module Shaker.Action.Compile
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


-- runCompileProject :: ReaderT ShakerConfig IO[String]
-- runCompileProject =  listProjectFiles >>= runCompile 

runCompile :: ShakerInput -> IO [String]
runCompile (ShakerInput (CompileInput procFlags) (ListenerInput fli _)) = do
        targetFiles <-  recurseListFiles fli
        defaultErrorHandler defaultDynFlags $ do
	runGhc (Just libdir) $ do
	dflags <- getSessionDynFlags
	setSessionDynFlags $ procFlags dflags 
        {-
        dflags {
          importPaths = ["src/","testsuite/tests/"], 
          verbosity = 1, 
          objectDir = Just "target",
          hiDir = Just "target",
          packageFlags = [ExposePackage "ghc"]
        }
        -}
	target <- mapM (\a -> guessTarget a Nothing) targetFiles
	setTargets target
	load LoadAllTargets
	g <- getModuleGraph
	mapM showModule g     

