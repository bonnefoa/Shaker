module Shaker.SourceHelper
 where

import Data.List 
import GHC
import DynFlags 
import GHC.Paths
import Shaker.Io
import Shaker.Type
import Control.Monad.Trans 
import Control.Monad.Reader

-- | Fill the target files to all files in listenerInput if empty
checkTargetFiles :: [String] -> Shaker IO([String])
checkTargetFiles [] = do 
        (ListenerInput fli _) <- asks listenerInput 
        files <- lift $ recurseMultipleListFiles fli
        lift $ filterM (\a -> not `liftM` isFileContainingMain a) files
checkTargetFiles l = return l


ghcCompile :: GhcMonad m => CompileInput -> [String] -> m SuccessFlag
ghcCompile (CompileInput sourceDir _ targetInput procFlags strflags _) targetFiles  = do   
     dflags <- getSessionDynFlags
     (newFlags,_,_) <- parseDynamicFlags dflags (map noLoc strflags)
     _ <- setSessionDynFlags $ procFlags $ setSourceAndTarget sourceDir targetInput newFlags
     target <- mapM (`guessTarget` Nothing) targetFiles
     setTargets target
     load LoadAllTargets

getCompileInputForAllHsSources :: Shaker IO CompileInput
getCompileInputForAllHsSources = do 
  cplInps@(cpIn:_) <- asks compileInputs
  let srcDirs = nub $ concatMap cfSourceDirs cplInps
  filePaths <- lift $ recurseMultipleListFiles $ map (\a -> FileListenInfo a defaultExclude defaultHaskellPatterns ) srcDirs 
  newTargets <-  lift $ filterM (\a -> not `liftM` isFileContainingMain a) filePaths  
  newTargetsWithoutMain <- checkTargetFiles newTargets
  return  $ cpIn {cfTargetFiles = newTargetsWithoutMain, cfDescription ="Full compilation"  }

setSourceAndTarget :: [String] -> String ->DynFlags -> DynFlags
setSourceAndTarget sources target dflags = dflags{
    importPaths = sources
    ,objectDir = Just target
    ,hiDir = Just target
  }

