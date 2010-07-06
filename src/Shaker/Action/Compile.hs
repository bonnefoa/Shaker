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

-- |Run haskell compilation on given file input 
runCompile :: Plugin
runCompile shakerInput = do
        targetFiles <-  recurseListFiles fli
        defaultErrorHandler defaultDynFlags $ action targetFiles
        where (CompileInput procFlags flags) = compileInput shakerInput
              (ListenerInput fli _) = listenerInput shakerInput
              action targetFiles = 
                       runGhc (Just libdir) $ do
                       dflags <- getSessionDynFlags
                       (newFlags,_,_) <- parseDynamicFlags dflags [noLoc flags]
	               setSessionDynFlags $ procFlags newFlags
                       target <- mapM (`guessTarget` Nothing) targetFiles
                       setTargets target
        	       load LoadAllTargets
                       return ()
 
