-- | Utilities function for compilation and haskell file management
module Shaker.SourceHelper(
  -- * Compile input management
  CompileFile(..)
  ,mergeCompileInputsSources
  ,constructCompileFileList
  -- * Target files filtering
  ,setAllHsFilesAsTargets
  ,removeFileWithMain
  ,removeFileWithTemplateHaskell
  ,fillCompileInputWithStandardTarget
  -- * GHC Compile management
  ,initializeGhc
  ,ghcCompile
  -- * module change detection
  ,checkUnchangedSources
  ,isModuleNeedCompilation
)
 where

import GHC
import Data.List
import Shaker.Io
import Shaker.Type

import Control.Monad.Reader(ask, Reader)

import LazyUniqFM
import MkIface 
import HscTypes

type CompileR = Reader [CompileFile]

data CompileFile = CompileFile {
  cfFp :: FilePath 
  ,cfHasMain :: Bool 
  ,cfHasTH :: Bool
 } deriving Show


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

-- | Fill compile input with every haskell files in the project except those
-- containing main and template haskell
fillCompileInputWithStandardTarget :: CompileInput -> CompileR CompileInput 
fillCompileInputWithStandardTarget cpIn = setAllHsFilesAsTargets cpIn >>= removeFileWithMain >>=removeFileWithTemplateHaskell

-- | Configure and load targets of compilation. 
-- It is possible to exploit the compilation result after this step.
ghcCompile :: GhcMonad m => CompileInput -> m SuccessFlag
ghcCompile cpIn = do   
     initializeGhc cpIn
     load LoadAllTargets

initializeGhc :: GhcMonad m => CompileInput -> m ()
initializeGhc cpIn@(CompileInput _ _ _ procFlags strflags targetFiles) = do   
     dflags <- getSessionDynFlags
     (newFlags,_,_) <- parseDynamicFlags dflags (map noLoc strflags)
     let chgdFlags = configureDynFlagsWithCompileInput cpIn newFlags
     _ <- setSessionDynFlags $ procFlags chgdFlags
     target <- mapM (`guessTarget` Nothing) targetFiles
     setTargets target

-- | Check of the module need to be recompile.
-- Modify ghc session by adding the module iface in the homePackageTable
isModuleNeedCompilation :: (GhcMonad m) => 
  [FilePath] -- ^ List of modified files
  -> ModSummary  -- ^ ModSummary to check
  -> m Bool -- ^ Result : is the module need to be recompiled
isModuleNeedCompilation modFiles ms = do
    hsc_env <- getSession
    (recom, mb_md_iface ) <- liftIO $ checkOldIface hsc_env ms source_unchanged Nothing
    case mb_md_iface of
        Just md_iface -> do 
                let module_name = (moduleName . mi_module) md_iface 
                    the_hpt = hsc_HPT hsc_env 
                    home_mod_info = HomeModInfo {hm_iface = md_iface, hm_details = emptyModDetails, hm_linkable = Nothing }
                    newHpt = addToUFM  the_hpt module_name home_mod_info
                modifySession (\h -> h {hsc_HPT = newHpt} )
                return recom 
        _ -> return True
  where source_unchanged = checkUnchangedSources modFiles ms

checkUnchangedSources :: [FilePath] -> ModSummary ->  Bool
checkUnchangedSources modifiedFiles ms = check hsSource
  where hsSource = (ml_hs_file . ms_location) ms
        check Nothing = False
        check (Just src) = not $ src `elem` modifiedFiles

