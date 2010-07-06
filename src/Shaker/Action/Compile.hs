module Shaker.Action.Compile
 where

import GHC
import DynFlags 
import GHC.Paths
import Shaker.Io
import Shaker.Type

-- |Run haskell compilation on given file input 
runCompile :: Plugin
runCompile shakerInput = do
        targetFiles <-  recurseListFiles fli
        defaultErrorHandler defaultDynFlags $ 
                       runGhc (Just libdir) $ do
                       dflags <- getSessionDynFlags
                       (newFlags,_,_) <- parseDynamicFlags dflags [noLoc strflags]
	               _ <- setSessionDynFlags $ procFlags newFlags
                       target <- mapM (`guessTarget` Nothing) targetFiles
                       setTargets target
        	       _ <- load LoadAllTargets
                       return ()
        where (CompileInput procFlags strflags) = compileInput shakerInput
              (ListenerInput fli _) = listenerInput shakerInput
 
