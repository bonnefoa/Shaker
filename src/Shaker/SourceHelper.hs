module Shaker.SourceHelper
 where

import Data.List 
import GHC
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
ghcCompile cpIn@(CompileInput _ _ _ procFlags strflags _) targetFiles  = do   
     dflags <- getSessionDynFlags
     (newFlags,_,_) <- parseDynamicFlags dflags (map noLoc strflags)
     _ <- setSessionDynFlags $ procFlags $ runReader (setSourceAndCompileTarget newFlags) cpIn 
     target <- mapM (`guessTarget` Nothing) targetFiles
     setTargets target
     load LoadAllTargets

removeFileWithTemplateHaskell :: [String] -> IO [String]
removeFileWithTemplateHaskell targetFiles = do 
  newTargets <-  filterM (\a -> not `liftM` isFileContainingMain a) $ targetFiles
  return  newTargets 

getCompileInputForAllHsSources :: Shaker IO CompileInput
getCompileInputForAllHsSources = do 
  cplInps@(cpIn:_) <- asks compileInputs
  let srcDirs = nub $ concatMap cfSourceDirs cplInps
  filePaths <- lift $ recurseMultipleListFiles $ map (\a -> FileListenInfo a defaultExclude defaultHaskellPatterns ) srcDirs 
  newTargets <-  lift $ filterM (\a -> not `liftM` isFileContainingMain a) filePaths  
  newTargetsWithoutMain <- checkTargetFiles newTargets
  return  $ cpIn {cfTargetFiles = newTargetsWithoutMain, cfDescription ="Full compilation"  }

setSourceAndCompileTarget :: DynFlags -> CompileM DynFlags 
setSourceAndCompileTarget dflags = do 
  sources <- asks cfSourceDirs
  compileTarget <- asks cfCompileTarget
  return dflags{
    importPaths = sources
    ,objectDir = Just compileTarget
    ,hiDir = Just compileTarget
  }

