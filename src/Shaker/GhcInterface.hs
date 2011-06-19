module Shaker.GhcInterface (
  -- * GHC Compile management
  initializeGhc
  ,ghcCompile
  ,getListNeededPackages
  ,installedPackageIdString
  ,fillModuleDataTest
  ,addLibraryToDynFlags
  ,searchInstalledPackageId
 )
 where

import Distribution.InstalledPackageInfo
import Distribution.Simple.PackageIndex
import Control.Arrow
import Control.Monad.Reader(lift, asks )
import Data.List
import Data.Monoid
import Data.Maybe
import Digraph
import Distribution.Package (InstalledPackageId(..))
import DynFlags
import GHC hiding (parseModule, HsModule)
import GHC.Paths
import HscTypes
import Linker
import Name (nameOccName)
import OccName (occNameString)
import Outputable
import Packages (lookupModuleInAllPackages, PackageConfig)
import qualified Data.Map as M
import Shaker.Io
import Shaker.Type
import Shaker.ModuleData
import Shaker.CommonUtil
import Var (varName)

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

-- * Test discovering

fillModuleDataTest :: [ModuleData] -> Shaker IO [[ModuleData]]
fillModuleDataTest = separateEqual
  >>> mapM fillModuleDataTest'

fillModuleDataTest' :: [ModuleData] -> Shaker IO [ModuleData]
fillModuleDataTest' modDatas = do
  cpIn <- fmap mconcat (asks shakerCompileInputs)
  let newCpIn = cpIn {
   compileInputTargetFiles = map moduleDataFileName modDatas
  }
  ghcModuleDatas <- lift $ runGhc (Just libdir) $ do
    _ <- ghcCompile newCpIn
    mss <- depanal [] False
    let sort_mss = flattenSCCs $ topSortModuleGraph True mss Nothing
    mapM convertModSummaryToModuleData sort_mss
  mergeMdatas
    >>> filter (\a -> moduleDataName a /= "")
    >>> removeNonTestModules
    >>> return $ (modDatas ++ ghcModuleDatas)

mergeMdatas :: [ModuleData] -> [ModuleData]
mergeMdatas lstMdatas = map (\mdata -> filter (==mdata) >>> mconcat $ lstMdatas) uniqueMdata
  where uniqueMdata = nub lstMdatas

-- | Collect module name and tests name for the given module
convertModSummaryToModuleData :: (GhcMonad m) => ModSummary -> m ModuleData
convertModSummaryToModuleData modSum = do
  mayModuleInfo <- getModuleInfo $ ms_mod modSum
  let assertions = getHunitAssertions mayModuleInfo
  let testCases  = getHunitTestCase mayModuleInfo
  return GhcModuleData {
    ghcModuleDataName        = modName
    ,ghcModuleDataAssertions = assertions
    ,ghcModuleDataTestCase   = testCases
    }
  where modName = (moduleNameString . moduleName . ms_mod) modSum

getHunitAssertions :: Maybe ModuleInfo -> [String]
getHunitAssertions = getFunctionTypeWithPredicate (== "Test.HUnit.Lang.Assertion")

getHunitTestCase :: Maybe ModuleInfo -> [String]
getHunitTestCase = getFunctionTypeWithPredicate (== "Test.HUnit.Base.Test")

getFunctionTypeWithPredicate :: (String -> Bool) -> Maybe ModuleInfo -> [String]
getFunctionTypeWithPredicate _ Nothing = []
getFunctionTypeWithPredicate predicat (Just modInfo) =
  getIdExportedList
  >>> map ((showPpr . idType) &&& getFunctionNameFromId )
  >>> filter (predicat . fst)
  >>> map snd $ modInfo

getFunctionNameFromId :: Id -> String
getFunctionNameFromId = occNameString . nameOccName . varName

getIdExportedList :: ModuleInfo -> [Id]
getIdExportedList modInfo = modInfoTyThings
  >>> mapMaybe tyThingToId
  >>> filter (\a -> varName a `elem` lstExportedNames)
  $ modInfo
  where lstExportedNames = modInfoExports modInfo

tyThingToId :: TyThing -> Maybe Id
tyThingToId (AnId tyId) = Just tyId
tyThingToId _ = Nothing

addLibraryToDynFlags :: [String] -> DynFlags -> DynFlags
addLibraryToDynFlags listInstalledPkgId dflags = dflags {
    packageFlags = nub $ map ExposePackageId listInstalledPkgId ++ oldPackageFlags
  }
  where oldPackageFlags = packageFlags dflags

searchInstalledPackageId :: String -> Shaker IO (Maybe String)
searchInstalledPackageId pkgName = do
  pkgIndex <- asks shakerPackageIndex
  let srchRes = searchByName pkgIndex pkgName
  return $ processSearchResult srchRes
  where processSearchResult None = Nothing
        processSearchResult (Unambiguous a) = Just $ installedPackageId >>> installedPackageIdString $ last a
        processSearchResult (Ambiguous (a:_)) = Just $ installedPackageId >>> installedPackageIdString $ last a
        processSearchResult _ = Nothing

