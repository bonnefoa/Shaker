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

-- |Run haskell compilation on given file input 
runCompile :: Plugin
runCompile shakerInput = do
        targetFiles <-  lift $ recurseMultipleListFiles fli
        lift $ defaultErrorHandler defaultDynFlags $ 
                       runGhc (Just libdir) $ do
                       dflags <- getSessionDynFlags
                       (newFlags,_,_) <- parseDynamicFlags dflags (map noLoc strflags)
	               _ <- setSessionDynFlags $ procFlags $ setSourceAndTarget sourceDir target newFlags
                       target <- mapM (`guessTarget` Nothing) targetFiles
                       setTargets target
        	       _ <- load LoadAllTargets
                       return ()
        where (CompileInput sourceDir target procFlags strflags) = compileInput shakerInput
              (ListenerInput fli _) = listenerInput shakerInput
 


setSourceAndTarget :: [String] -> String ->DynFlags -> DynFlags
setSourceAndTarget sources target dflags = dflags{
    importPaths = sources
    ,objectDir = Just target
    ,hiDir = Just target
  }

