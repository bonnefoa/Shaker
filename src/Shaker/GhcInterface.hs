module Shaker.GhcInterface (
  -- * GHC Compile management
  initializeGhc
  ,ghcCompile
  ,getListNeededPackages
  ,installedPackageIdString 
 )
 where

import Control.Arrow
import Control.Monad.Reader(lift, asks )
import Data.List
import Distribution.Package (InstalledPackageId(..))
import DynFlags
import GHC hiding (parseModule, HsModule)
import GHC.Paths
import Linker
import HscTypes
import Packages (lookupModuleInAllPackages, exposed,  installedPackageId, PackageConfig)
import qualified Data.Map as M
import Shaker.Io
import Shaker.Type

type ImportToPackages = [ ( String, [PackageConfig] ) ]

-- | Get the list of unresolved import and 
-- unexposed yet needed packages
getListNeededPackages :: Shaker IO [String]
getListNeededPackages = do
  cpIn <- fmap head (asks shakerCompileInputs)
  (PackageData map_import_modules list_project_modules) <- lift mapImportToModules
  import_to_packages <- lift $ runGhc (Just libdir) $ do 
    initializeGhc cpIn
    dyn_flags <- getSessionDynFlags
    return $ map ( \ imp -> (imp , lookupModuleInAllPackages dyn_flags . mkModuleName $ imp) ) 
              >>> map ( second (map fst) )
              $ (M.keys map_import_modules \\ list_project_modules) 
  return $ getPackagesToExpose import_to_packages

getPackagesToExpose :: ImportToPackages -> [String]
getPackagesToExpose = map snd
    >>> filter (not . null)
    >>> filter (all (not . exposed) ) 
    >>> map head 
    >>> nubBy (\a b ->  getPackage a == getPackage b ) 
    >>> filter (not . exposed)
    >>> map getPackage 
  where getPackage = installedPackageId >>> installedPackageIdString

installedPackageIdString :: InstalledPackageId -> String
installedPackageIdString (InstalledPackageId v) = v 

initializeGhc :: GhcMonad m => CompileInput -> m ()
initializeGhc cpIn@(CompileInput _ _ procFlags strflags targetFiles) = do   
     modifySession (\h -> h {hsc_HPT = emptyHomePackageTable} )
     dflags <- getSessionDynFlags
     (newFlags,_,_) <- parseDynamicFlags dflags (map noLoc strflags)
     let chgdFlags = configureDynFlagsWithCompileInput cpIn newFlags
     _ <- setSessionDynFlags $ procFlags chgdFlags
     target <- mapM (`guessTarget` Nothing) targetFiles
     setTargets target

-- | Configure and load targets of compilation. 
-- It is possible to exploit the compilation result after this step.
ghcCompile :: GhcMonad m => CompileInput -> m SuccessFlag
ghcCompile cpIn = do   
     initializeGhc cpIn
     dflags <- getSessionDynFlags
     liftIO $ unload dflags []
     load LoadAllTargets

-- | Change the dynflags with information from the CompileInput like importPaths 
-- and .o and .hi fileListenInfoDirectory
configureDynFlagsWithCompileInput :: CompileInput -> DynFlags -> DynFlags 
configureDynFlagsWithCompileInput cpIn dflags = dflags{
    importPaths = sourceDirs
    ,objectDir = Just compileTarget
    ,hiDir = Just compileTarget
  }
  where compileTarget = compileInputBuildDirectory cpIn
        sourceDirs = compileInputSourceDirs cpIn

