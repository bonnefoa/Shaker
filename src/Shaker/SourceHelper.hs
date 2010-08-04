module Shaker.SourceHelper
 where

import GHC
import Data.List
import Shaker.Io
import Shaker.Type
import Control.Monad.Reader

type CompileR = Reader [CompileFile]

data CompileFile = CompileFile {
  cfFp :: FilePath 
  ,cfHasMain :: Bool 
  ,cfHasTH :: Bool
 } deriving Show

-- * Compile input management

-- | Build the list of haskell source files located in 
-- CompileInput source dirs
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

-- | Merge source dirs informations from the CompileInput list to 
-- create a single CompileInput
mergeCompileInputsSources :: [CompileInput] -> CompileInput
mergeCompileInputsSources [] = defaultCompileInput 
mergeCompileInputsSources cplInps@(cpIn:_) = do 
  let srcDirs = nub $ concatMap cfSourceDirs cplInps
  cpIn {cfSourceDirs = srcDirs, cfDescription ="Full compilation"  } 

-- | Fill the target files to all files in listenerInput if empty
fillTargetIfEmpty ::CompileInput -> CompileR CompileInput
fillTargetIfEmpty cpIn = if null (cfTargetFiles cpIn) 
     then setAllHsFilesAsTargets cpIn
     else return cpIn

-- | Configure the CompileInput with all haskell files configured as targets
setAllHsFilesAsTargets :: CompileInput -> CompileR CompileInput
setAllHsFilesAsTargets cpIn = do
  files <- ask
  return cpIn {cfTargetFiles = map cfFp files }

-- | Change the dynflags with information from the CompileInput like importPaths 
-- and .o and .hi directory
configureDynFlagsWithCompileInput :: CompileInput -> DynFlags -> DynFlags 
configureDynFlagsWithCompileInput cpIn dflags = dflags{
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

