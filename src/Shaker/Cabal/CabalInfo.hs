-- | Allow to use cabal configuration (generated via the configure action of cabal).
-- Source fileListenInfoDirectories and compilation options will be reused by Shaker.
module Shaker.Cabal.CabalInfo(
    defaultCabalInput
  )
   where

import Shaker.Type
import Shaker.Config
import Shaker.GhcInterface

import Distribution.Simple.Build
import Distribution.Verbosity
import Distribution.Simple.Configure (maybeGetPersistBuildConfig, configure, writePersistBuildConfig)
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription
import Distribution.Simple.Utils
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo 
import Distribution.ModuleName
import Distribution.Simple.Setup
import DynFlags(
    DynFlags, verbosity, ghcLink, packageFlags, hiDir, objectDir ,importPaths
    ,PackageFlag (ExposePackageId)
    ,GhcLink (NoLink)
  )
import Distribution.Compiler(CompilerFlavor(GHC))
import Distribution.Package (PackageName(PackageName), pkgName)

import System.FilePath          ( (</>))
import System.Directory (doesFileExist)

import Data.Maybe
import Data.List(nub,isSuffixOf, find, isPrefixOf)
import Data.Monoid 

import Shaker.ModuleData

import Control.Monad.Reader
import Control.Arrow

-- | Read the build information from cabal and output a shakerInput from it
defaultCabalInput :: IO ShakerInput
defaultCabalInput = readConf >>= \lbi -> 
  generatePreprocessFile lbi >> 
  localBuildInfoToShakerInput lbi >>= exposeNeededPackages lbi >>= checkInvalidMain >>= fillModuleData

generatePreprocessFile :: LocalBuildInfo -> IO ()
generatePreprocessFile lbi = writeAutogenFiles normal (localPkgDescr lbi) lbi

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
  return defInput {
    shakerCompileInputs = cplInputs
    ,shakerListenerInput= compileInputsToListenerInput cplInputs
    ,shakerLocalBuildInfo = lbi
  }
  where cplInputs = localBuildInfoToCompileInputs  lbi

compileInputsToListenerInput :: [CompileInput] -> ListenerInput
compileInputsToListenerInput cplInputs = mempty {
        listenerInputFiles = nub $ map (\a -> FileListenInfo a defaultExclude  defaultHaskellPatterns) concatSources
 } 
 where concatSources = concatMap compileInputSourceDirs cplInputs
       
-- * Converter to CompileInput

-- | Extract informations : Convert executable and library to 
-- compile inputs
localBuildInfoToCompileInputs  :: LocalBuildInfo -> [CompileInput]
localBuildInfoToCompileInputs  lbi = executableAndLibToCompileInput libraryTuple  executablesTuples
 where pkgDescription = localPkgDescr lbi
       libraryTuple = library pkgDescription >>= \a ->  libraryConfig lbi >>= \b -> return (a,b)
       executablesTuples = mapMaybe ( \ (name, comp) -> find (\ex -> exeName ex == name) listExecutables >>= \e -> return (e, comp) ) listConfigs
       listExecutables = executables pkgDescription
       listConfigs = executableConfigs lbi

-- | Dispatch the processing depending of the library content
executableAndLibToCompileInput :: Maybe (Library, ComponentLocalBuildInfo )-> [(Executable,ComponentLocalBuildInfo)] -> [CompileInput]
executableAndLibToCompileInput Nothing exes = map executableToCompileInput exes
executableAndLibToCompileInput (Just lib) exes = libraryToCompileInput lib : map executableToCompileInput exes

-- | Convert a cabal executable to a compileInput
-- The target of compilation will the main file
executableToCompileInput :: (Executable, ComponentLocalBuildInfo) -> CompileInput
executableToCompileInput (executable, componentLocalBuildInfo) = mempty { 
  compileInputSourceDirs = mySourceDir
  ,compileInputCommandLineFlags = getCompileOptions bldInfo
  ,compileInputTargetFiles = map (</> modulePath executable ) mySourceDir
  ,compileInputDynFlags = toDynFlags mySourceDir (getLibDependencies componentLocalBuildInfo)
  }
  where bldInfo = buildInfo executable
        mySourceDir = "dist/build/autogen" : hsSourceDirs bldInfo

-- | Convert a cabal library to a compileInput
-- The target of compilation will be all exposed modules
libraryToCompileInput :: (Library, ComponentLocalBuildInfo) -> CompileInput
libraryToCompileInput (lib, componentLocalBuildInfo) = mempty {
  compileInputSourceDirs = mySourceDir
  ,compileInputCommandLineFlags = getCompileOptions bldInfo
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
  ,objectDir = Just "dist/shakerTarget"
  ,hiDir = Just "dist/shakerTarget"
  ,verbosity = 1
  ,ghcLink = NoLink
  ,packageFlags = nub $ map ExposePackageId packagesToExpose ++ oldPackageFlags
  } 
  where oldPackageFlags = packageFlags dnFlags
        oldImportPaths = importPaths dnFlags

-- * Helper methods

getCompileOptions :: BuildInfo -> [String]
getCompileOptions myLibBuildInfo = hideAllPackagesOption : ghcOptions ++ ghcExtensions
 where ghcOptions = fromMaybe [] $ lookup GHC (options myLibBuildInfo)
       ghcExtensions = map (\a -> "-X"++ show a) (extensions myLibBuildInfo)
       hideAllPackagesOption = "-hide-all-packages"

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


