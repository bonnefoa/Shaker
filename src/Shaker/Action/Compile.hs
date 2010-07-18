module Shaker.Action.Compile(
    runCompile
    ,runFullCompile
  )
 where

import Data.List 
import GHC
import DynFlags 
import GHC.Paths
import Shaker.Io
import Shaker.Type
import Control.Monad.Trans 
import Control.Monad.Reader

-- |Run haskell compilation on given file input 
runCompile :: Plugin
runCompile = asks compileInputs >>=  mapM runSingleCompileInput >> return ()

runSingleCompileInput :: CompileInput -> Shaker IO()
runSingleCompileInput (CompileInput sourceDir desc targetInput procFlags strflags inputTargetFiles) = do
        lift $ putStrLn $ concat ["--------- ", desc," ---------"]
        targetFiles <- checkTargetFiles inputTargetFiles 
        lift $ defaultErrorHandler defaultDynFlags $ 
                       runGhc (Just libdir) $ do
                       dflags <- getSessionDynFlags
                       (newFlags,_,_) <- parseDynamicFlags dflags (map noLoc strflags)
	               _ <- setSessionDynFlags $ procFlags $ setSourceAndTarget sourceDir targetInput newFlags
                       target <- mapM (`guessTarget` Nothing) targetFiles
                       setTargets target
        	       _ <- load LoadAllTargets
                       return ()
 
runFullCompile :: Plugin
runFullCompile = setCompileInputForAllHsSources >>= \a -> 
  runSingleCompileInput a >> 
  return () 

-- | Fill the target files to all files in listenerInput if empty
checkTargetFiles :: [String] -> Shaker IO([String])
checkTargetFiles [] = do 
        (ListenerInput fli _) <- asks listenerInput 
        lift $ recurseMultipleListFiles fli
checkTargetFiles l = return l

setSourceAndTarget :: [String] -> String ->DynFlags -> DynFlags
setSourceAndTarget sources target dflags = dflags{
    importPaths = sources
    ,objectDir = Just target
    ,hiDir = Just target
  }

setCompileInputForAllHsSources :: Shaker IO (CompileInput)
setCompileInputForAllHsSources = do 
  (cpIn:_) <- asks compileInputs
  filePaths <- lift $ recurseMultipleListFiles $ map (\a -> FileListenInfo a defaultExclude defaultHaskellPatterns ) (cfSourceDirs $ cpIn)
  toExcludeFiles <- lift $ filterM (isFileContainingMain) filePaths
  return  $ cpIn {cfTargetFiles = filePaths \\ toExcludeFiles , cfDescription ="Full compilation"  }


