module Shaker.SourceHelper
 where

import Data.List 
import GHC
import Shaker.Io
import Shaker.Type
import Control.Monad.Trans 
import Control.Monad.Reader

-- | Fill the target files to all files in listenerInput if empty
fillTargetIfEmpty :: CompileInput -> IO CompileInput
fillTargetIfEmpty cpIn
  | null targetFiles = do 
        files <- recurseMultipleListFiles fli
        return cpIn { cfTargetFiles = files}
  | otherwise = return cpIn
  where targetFiles = cfTargetFiles cpIn
        fli = getFileListenInfoForCompileInput cpIn

ghcCompile :: GhcMonad m => CompileInput -> m SuccessFlag
ghcCompile cpIn@(CompileInput _ _ _ procFlags strflags targetFiles) = do   
     dflags <- getSessionDynFlags
     (newFlags,_,_) <- parseDynamicFlags dflags (map noLoc strflags)
     let chgdFlags = configureDynFlagsWithCompileInput cpIn newFlags
     _ <- setSessionDynFlags $ procFlags chgdFlags
     target <- mapM (`guessTarget` Nothing) targetFiles
     setTargets target
     load LoadAllTargets

setAllHsFilesAsTargets :: CompileInput -> IO CompileInput
setAllHsFilesAsTargets cpIn = do
  targets <- recurseMultipleListFiles $ map (\a -> FileListenInfo a defaultExclude defaultHaskellPatterns ) srcDirs 
  return cpIn {cfTargetFiles = targets}
  where srcDirs = cfSourceDirs cpIn

getCompileInputForAllHsSources :: Shaker IO CompileInput
getCompileInputForAllHsSources = do 
  cplInps@(cpIn:_) <- asks compileInputs
  let srcDirs = nub $ concatMap cfSourceDirs cplInps
  cpInRes <- lift $ setAllHsFilesAsTargets cpIn {cfSourceDirs = srcDirs} 
  return $ cpInRes {cfDescription ="Full compilation"  }

configureDynFlagsWithCompileInput :: CompileInput -> DynFlags -> DynFlags 
configureDynFlagsWithCompileInput cpIn dflags = do 
  dflags{
    importPaths = sourceDirs
    ,objectDir = Just compileTarget
    ,hiDir = Just compileTarget
  }
  where compileTarget = cfCompileTarget cpIn
        sourceDirs = cfSourceDirs cpIn

getFileListenInfoForCompileInput :: CompileInput -> [FileListenInfo] 
getFileListenInfoForCompileInput cpIn =
  map (\a -> FileListenInfo a defaultExclude defaultHaskellPatterns) (cfSourceDirs cpIn)

-- * Target files filtering

removeFileWithTemplateHaskell :: CompileInput -> IO CompileInput
removeFileWithTemplateHaskell = removeFileWithPredicate (\a -> not `liftM` isFileContainingTH a) 

removeFileWithMain :: CompileInput -> IO CompileInput
removeFileWithMain = removeFileWithPredicate (\a -> not `liftM` isFileContainingMain a) 

removeFileWithPredicate :: (String -> IO Bool) -> CompileInput -> IO CompileInput 
removeFileWithPredicate predicate cpIn = do
  let targetFiles = cfTargetFiles cpIn
  filteredTargets <- filterM predicate targetFiles
  return cpIn {cfTargetFiles = filteredTargets}

