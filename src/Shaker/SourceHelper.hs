module Shaker.SourceHelper
 where

import GHC
import Data.List
import Shaker.Io
import Shaker.Type
import Control.Monad.Trans 
import Control.Monad.Reader

type CompileR = Reader [CompileFile]

{-
data CompileState = CompileState {
  csCpIn:: CompileInput
 ,csCpFl :: [CompileFile] 
}
-}

data CompileFile = CompileFile {
  cfFp :: FilePath 
  ,cfHasMain :: Bool 
  ,cfHasTH :: Bool
 }

-- * Compile input management

constructCompileFileList :: CompileInput -> IO [CompileFile] 
constructCompileFileList cpIn = do
  files <- recurseMultipleListFiles fli
  mapM constructCompileFile files
  where fli = getFileListenInfoForCompileInput cpIn
  
constructCompileFile :: FilePath -> IO CompileFile      
constructCompileFile fp = do
  hasMain <- isFileContainingMain fp
  hasTH <- isFileContainingTH fp
  return $ CompileFile fp hasMain hasTH

mergeCompileInputsSources :: Shaker IO CompileInput
mergeCompileInputsSources = do 
  cplInps@(cpIn:_) <- asks compileInputs
  let srcDirs = nub $ concatMap cfSourceDirs cplInps
  return $  cpIn {cfSourceDirs = srcDirs, cfDescription ="Full compilation"  } 

-- | Fill the target files to all files in listenerInput if empty
fillTargetIfEmpty ::CompileInput -> CompileR CompileInput
fillTargetIfEmpty cpIn = do
  if null (cfTargetFiles cpIn) 
     then setAllHsFilesAsTargets cpIn
     else return cpIn

setAllHsFilesAsTargets :: CompileInput -> CompileR CompileInput
setAllHsFilesAsTargets cpIn = do
  files <- ask
  return cpIn {cfTargetFiles = map cfFp files }

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

removeFileWithTemplateHaskell :: CompileInput ->CompileR CompileInput
removeFileWithTemplateHaskell = removeFileWithPredicate cfHasTH

removeFileWithMain :: CompileInput -> CompileR CompileInput
removeFileWithMain = removeFileWithPredicate cfHasMain

removeFileWithPredicate :: (CompileFile -> Bool) -> CompileInput -> CompileR CompileInput
removeFileWithPredicate predicate cpIn = do 
  cpFl <- ask 
  let toRemove = map cfFp $ filter predicate cpFl
  return $ cpIn {cfTargetFiles =  targets \\ toRemove}
  where targets = cfTargetFiles cpIn

-- * GHC Compile management

ghcCompile :: GhcMonad m => CompileInput -> m SuccessFlag
ghcCompile cpIn@(CompileInput _ _ _ procFlags strflags targetFiles) = do   
     dflags <- getSessionDynFlags
     (newFlags,_,_) <- parseDynamicFlags dflags (map noLoc strflags)
     let chgdFlags = configureDynFlagsWithCompileInput cpIn newFlags
     _ <- setSessionDynFlags $ procFlags chgdFlags
     target <- mapM (`guessTarget` Nothing) targetFiles
     setTargets target
     load LoadAllTargets

