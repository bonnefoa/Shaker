module Shaker.Action.Compile(
    runCompile
  )
 where

import GHC
import DynFlags 
import GHC.Paths
import Shaker.Io
import Shaker.Type
import Control.Monad.Trans 
import Control.Monad.Reader

-- |Run haskell compilation on given file input 
runCompile :: Plugin
runCompile = do
        (CompileInput sourceDir targetInput procFlags strflags) <-  asks compileInput 
        (ListenerInput fli _) <- asks listenerInput 
        targetFiles <-  lift $ recurseMultipleListFiles fli
        lift $ defaultErrorHandler defaultDynFlags $ 
                       runGhc (Just libdir) $ do
                       dflags <- getSessionDynFlags
                       (newFlags,_,_) <- parseDynamicFlags dflags (map noLoc strflags)
	               _ <- setSessionDynFlags $ procFlags $ setSourceAndTarget sourceDir targetInput newFlags
                       target <- mapM (`guessTarget` Nothing) targetFiles
                       setTargets target
        	       _ <- load LoadAllTargets
                       return ()
 


setSourceAndTarget :: [String] -> String ->DynFlags -> DynFlags
setSourceAndTarget sources target dflags = dflags{
    importPaths = sources
    ,objectDir = Just target
    ,hiDir = Just target
  }

