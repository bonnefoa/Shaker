-- | Allow to use cabal configuration (generated via the configure action of cabal).
-- Source fileListenInfoDirectories and compilation options will be reused by Shaker.
module Shaker.CabalInfo(
    defaultCabalInput
    ,applyPreprocessSources
  )
   where

import Control.Arrow
import Control.Monad.Reader
import Data.List(nub,isSuffixOf, find, isPrefixOf)
import Data.Maybe
import Data.Monoid 
import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Package (PackageName(PackageName), pkgName)
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.Simple.Configure (maybeGetPersistBuildConfig, configure, writePersistBuildConfig, getInstalledPackages)
import Distribution.Simple.LocalBuildInfo 
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Verbosity
import DynFlags( DynFlags, verbosity, ghcLink, packageFlags, importPaths ,PackageFlag (ExposePackageId) ,GhcLink (NoLink))
import Shaker.CabalInterface
import Shaker.Config
import Shaker.GhcInterface
import Shaker.Io
import Shaker.ModuleData
import Shaker.Type
import System.Directory (doesFileExist)
import System.FilePath          ( (</>))

-- | Read the build information from cabal and output a shakerInput from it
defaultCabalInput :: IO ShakerInput
defaultCabalInput = readConf 
  >>= \lbi -> generateAutogenFiles lbi 
  >> localBuildInfoToShakerInput lbi 
  >>= exposeNeededPackages lbi 
  >>= checkInvalidMain 
  >>= fillModuleData 
  >>= fillPackageIndex

readConf :: IO LocalBuildInfo
readConf = maybeGetPersistBuildConfig "dist" >>= \my_lbi ->
  case my_lbi of
   Just lbi -> return lbi
   Nothing -> callConfigure

callConfigure :: IO LocalBuildInfo
callConfigure = do
  genericPackageDescription <- defaultPackageDesc silent >>= readPackageDescription silent 
  lbi <- configure (genericPackageDescription ,emptyHookedBuildInfo) (defaultConfigFlags defaultProgramConfiguration) 
  writePersistBuildConfig "dist" lbi
  return lbi

-- | Extract useful information from localBuildInfo to a ShakerInput
localBuildInfoToShakerInput :: LocalBuildInfo -> IO ShakerInput
localBuildInfoToShakerInput lbi = do 
  defInput <- defaultInputInitialized 
  let cplInputs = localBuildInfoToCompileInputs lbi 
  let listenerInput = compileInputsToListenerInput cplInputs
  return defInput {
    shakerCompileInputs   = cplInputs
    ,shakerListenerInput  = listenerInput
    ,shakerLocalBuildInfo = lbi
  }

compileInputsToListenerInput :: [CompileInput] -> ListenerInput
compileInputsToListenerInput cplInputs = mempty {
        listenerInputFiles = nub $ map (\a -> FileListenInfo a defaultExclude defaultHaskellPatterns) concatSources
 } 
 where concatSources = concatMap compileInputSourceDirs cplInputs
       
-- * Converter to CompileInput

-- | Extract informations : Convert executable and library to 
-- compile inputs
localBuildInfoToCompileInputs  :: LocalBuildInfo -> [CompileInput]
localBuildInfoToCompileInputs lbi = executableAndLibToCompileInput lbi libraryTuple  executablesTuples
 where pkgDescription = localPkgDescr lbi
       libraryTuple = library pkgDescription >>= \a ->  libraryConfig lbi >>= \b -> return (a,b)
       executablesTuples = mapMaybe ( \ (name, comp) -> find (\ex -> exeName ex == name) listExecutables >>= \e -> return (e, comp) ) listConfigs
       listExecutables = executables pkgDescription
       listConfigs = executableConfigs lbi

-- | Dispatch the processing depending of the library content
executableAndLibToCompileInput :: LocalBuildInfo -> Maybe (Library, ComponentLocalBuildInfo) -> [(Executable,ComponentLocalBuildInfo)] -> [CompileInput]
executableAndLibToCompileInput lbi Nothing exes = map (executableToCompileInput lbi) exes
executableAndLibToCompileInput lbi (Just lib) exes = libraryToCompileInput lbi lib : map (executableToCompileInput lbi) exes

-- | Convert a cabal executable to a compileInput
-- The target of compilation will the main file
executableToCompileInput :: LocalBuildInfo -> (Executable, ComponentLocalBuildInfo) -> CompileInput
executableToCompileInput lbi (executable, componentLocalBuildInfo) = mempty { 
  compileInputSourceDirs = mySourceDir
  ,compileInputCommandLineFlags = getCompileFlagsForExecutable lbi executable componentLocalBuildInfo
  ,compileInputTargetFiles = map (</> modulePath executable ) mySourceDir
  ,compileInputDynFlags = toDynFlags mySourceDir (getLibDependencies componentLocalBuildInfo)
  }
  where bldInfo = buildInfo executable
        mySourceDir = "dist/build/autogen" : hsSourceDirs bldInfo

-- | Convert a cabal library to a compileInput
-- The target of compilation will be all exposed modules
libraryToCompileInput :: LocalBuildInfo -> (Library, ComponentLocalBuildInfo) -> CompileInput
libraryToCompileInput lbi (lib, componentLocalBuildInfo) = mempty {
  compileInputSourceDirs = mySourceDir
  ,compileInputCommandLineFlags = getCompileFlagsForLibrary lbi lib componentLocalBuildInfo
  ,compileInputTargetFiles = myModules
  ,compileInputDynFlags = toDynFlags mySourceDir (getLibDependencies componentLocalBuildInfo)
 }
 where bldInfo = libBuildInfo lib
       myModules = map convertModuleNameToString $ exposedModules lib
       mySourceDir = "dist/build/autogen": hsSourceDirs bldInfo 

-- | Create a dynFlags for ghc from a source fileListenInfoDirectory and 
-- a liste of packages
toDynFlags :: [String] -> [String] -> DynFlags -> DynFlags
toDynFlags sourceDirs packagesToExpose dnFlags = dnFlags {
  importPaths = nub $ oldImportPaths ++ sourceDirs
  ,verbosity = 1
  ,ghcLink = NoLink
  ,packageFlags = nub $ map ExposePackageId packagesToExpose ++ oldPackageFlags
  } 
  where oldPackageFlags = packageFlags dnFlags
        oldImportPaths = importPaths dnFlags

-- * Helper methods

getLibDependencies :: ComponentLocalBuildInfo -> [String]
getLibDependencies = componentPackageDeps >>> map (fst >>> installedPackageIdString ) 

convertModuleNameToString :: ModuleName -> String
convertModuleNameToString modName
 | null modArr = ""
 | otherwise = foldr1 (\w s -> w ++ '.':s) modArr
   where modArr = components modName 


-- | Check and filter all invalid main definission
checkInvalidMain :: ShakerInput -> IO ShakerInput 
checkInvalidMain shIn = mapM checkInvalidMain' (shakerCompileInputs  shIn) >>= \newCplInp ->
  return $ shIn {shakerCompileInputs = newCplInp  }

checkInvalidMain' :: CompileInput -> IO CompileInput
checkInvalidMain' cplInput
 | any (".hs" `isSuffixOf`) oldTargets = do
    newTargets <- filterM doesFileExist oldTargets
    return cplInput {compileInputTargetFiles = newTargets}
 | otherwise = return cplInput
  where oldTargets = compileInputTargetFiles cplInput

-- | Expose needed package
exposeNeededPackages :: LocalBuildInfo -> ShakerInput -> IO ShakerInput 
exposeNeededPackages lbi shIn = do
  listPackages <- runReaderT getListNeededPackages shIn
  putStrLn $ "Exposing packages " ++ show listPackages
  let packageFlagsToAdd = map ExposePackageId $ filter ( \ name -> not $ currentPackage `isPrefixOf` name ) listPackages
  let oldListenerInput = shakerListenerInput shIn
  let listenerInputFilesToMerge = mempty 
  let newCpIns = map ( \a -> mappend a $ mempty { compileInputDynFlags = addPackageToDynFlags packageFlagsToAdd } ) (shakerCompileInputs shIn)
  let newListFileListenInfo = map ( `mappend` listenerInputFilesToMerge) (listenerInputFiles oldListenerInput )
  let newListenerInput = oldListenerInput { listenerInputFiles = newListFileListenInfo }
  return $ shIn {shakerCompileInputs = newCpIns, shakerListenerInput= newListenerInput }
  where addPackageToDynFlags packageFlagToAdd dynFlags = dynFlags {
            packageFlags = packageFlags dynFlags ++ packageFlagToAdd
          } 
        currentPackage = localPkgDescr >>> package >>> pkgName >>> unPackageName $ lbi
        unPackageName (PackageName v) = v

fillPackageIndex :: ShakerInput -> IO ShakerInput 
fillPackageIndex shIn = do
  (Just pkgIndex) <- getInstalledPackages normal lbi_compiler [GlobalPackageDB] lbi_programConfiguration 
  return shIn { shakerPackageIndex = pkgIndex }
  where lbi_compiler = shakerLocalBuildInfo >>> compiler $ shIn
        lbi_programConfiguration = shakerLocalBuildInfo >>> withPrograms $ shIn

