-- | Allow to use cabal configuration (generated via the configure action of cabal).
-- Source directories and compilation options will be reused by Shaker.
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
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr)
import Distribution.ModuleName
import Distribution.Simple.Setup
import DynFlags(
    DynFlags, verbosity, ghcLink, packageFlags, outputFile, hiDir, objectDir ,importPaths
    ,PackageFlag (ExposePackage)
    ,GhcLink (NoLink)
  )
import Distribution.Compiler(CompilerFlavor(GHC))
import Distribution.Package (Dependency(Dependency), PackageName(PackageName), pkgName)

import System.FilePath          ( (</>))
import System.Directory (doesFileExist)

import Data.Maybe
import Data.List(nub,isSuffixOf, delete)
import Data.Monoid 

import Control.Monad.Reader
import Control.Arrow

-- | Read the build information from cabal and output a shakerInput from it
defaultCabalInput :: IO ShakerInput
defaultCabalInput = readConf >>= \lbi -> 
  generatePreprocessFile lbi >> 
  localBuildInfoToShakerInput lbi >>= exposeNeededPackages lbi >>= checkInvalidMain 

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
    compileInputs = cplInputs
    ,listenerInput = compileInputsToListenerInput cplInputs
  }
  where cplInputs = localBuildInfoToCompileInputs lbi

compileInputsToListenerInput :: [CompileInput] -> ListenerInput
compileInputsToListenerInput cplInputs = mempty {
        fileListenInfo = nub $ map (\a -> FileListenInfo a defaultExclude  defaultHaskellPatterns) concatSources
 } 
 where concatSources = concatMap cfSourceDirs cplInputs
       
-- * Converter to CompileInput

-- | Extract informations : Convert executable and library to 
-- compile inputs
localBuildInfoToCompileInputs :: LocalBuildInfo -> [CompileInput]
localBuildInfoToCompileInputs lbi = executableAndLibToCompileInput (library pkgDescription) (executables pkgDescription)
 where pkgDescription = localPkgDescr lbi


-- | Dispatch the processing depending of the library content
executableAndLibToCompileInput :: Maybe Library -> [Executable] -> [CompileInput]
executableAndLibToCompileInput Nothing exes = map executableToCompileInput exes
executableAndLibToCompileInput (Just lib) exes = libraryToCompileInput lib : map executableToCompileInput exes

-- | Convert a cabal executable to a compileInput
-- The target of compilation will the main file
executableToCompileInput :: Executable -> CompileInput
executableToCompileInput executable = mempty { 
  cfSourceDirs = mySourceDir
  ,cfDescription = "Executable : " ++ exeName executable
  ,cfCommandLineFlags = getCompileOptions bldInfo
  ,cfTargetFiles = map (</> modulePath executable ) mySourceDir
  ,cfDynFlags = toDynFlags mySourceDir (getLibDependencies bldInfo)
  }
  where bldInfo = buildInfo executable
        mySourceDir = "dist/build/autogen" : hsSourceDirs bldInfo

-- | Convert a cabal library to a compileInput
-- The target of compilation will be all exposed modules
libraryToCompileInput :: Library -> CompileInput
libraryToCompileInput lib = mempty {
  cfSourceDirs = mySourceDir
  ,cfDescription = "Library : " ++ show myModules
  ,cfCommandLineFlags = getCompileOptions bldInfo
  ,cfTargetFiles = myModules
  ,cfDynFlags = toDynFlags mySourceDir (getLibDependencies bldInfo)
 }
 where bldInfo = libBuildInfo lib
       myModules = map convertModuleNameToString $ exposedModules lib
       mySourceDir = "dist/build/autogen": hsSourceDirs bldInfo 

-- | Create a dynFlags for ghc from a source directory and 
-- a liste of packages
toDynFlags :: [String] -> [String] -> DynFlags -> DynFlags
toDynFlags sourceDirs packagesToExpose dnFlags = dnFlags {
  importPaths = nub $ oldImportPaths ++ sourceDirs
  ,outputFile = Just "dist/shakerTarget/Main"
  ,objectDir = Just "dist/shakerTarget"
  ,hiDir = Just "dist/shakerTarget"
  ,verbosity = 1
  ,ghcLink = NoLink
  ,packageFlags = nub $ map ExposePackage packagesToExpose ++ oldPackageFlags
  } 
  where oldPackageFlags = packageFlags dnFlags
        oldImportPaths = importPaths dnFlags

-- * Helper methods

getCompileOptions :: BuildInfo -> [String]
getCompileOptions myLibBuildInfo = hideAllPackagesOption : ghcOptions ++ ghcExtensions
 where ghcOptions = fromMaybe [] $ lookup GHC (options myLibBuildInfo)
       ghcExtensions = map (\a -> "-X"++ show a) (extensions myLibBuildInfo)
       hideAllPackagesOption = "-hide-all-packages"

getLibDependencies :: BuildInfo -> [String]
getLibDependencies bi = map getPackageName $ targetBuildDepends bi 

getPackageName :: Dependency -> String
getPackageName (Dependency (PackageName pn) _) = pn

convertModuleNameToString :: ModuleName -> String
convertModuleNameToString modName
 | null modArr = ""
 | otherwise = foldr1 (\w s -> w ++ '.':s) modArr
   where modArr = components modName 


-- | Check and filter all invalid main definission
checkInvalidMain :: ShakerInput -> IO ShakerInput 
checkInvalidMain shIn = mapM checkInvalidMain' (compileInputs shIn) >>= \newCplInp ->
  return $ shIn {compileInputs = newCplInp  }

checkInvalidMain' :: CompileInput -> IO CompileInput
checkInvalidMain' cplInput
 | any (".hs" `isSuffixOf`) oldTargets = do
    newTargets <- filterM doesFileExist oldTargets
    return cplInput {cfTargetFiles = newTargets}
 | otherwise = return cplInput
  where oldTargets = cfTargetFiles cplInput

-- | Expose needed package
exposeNeededPackages :: LocalBuildInfo -> ShakerInput -> IO ShakerInput 
exposeNeededPackages lbi shIn = do
  (ignoreModules, listPackages) <- runReaderT getListNeededPackages shIn
  putStrLn $ "Ignoring modules " ++ show ignoreModules
  putStrLn $ "Exposing " ++ show listPackages
  let packageToImport = delete currentPackage listPackages
  let cpIns = compileInputs shIn
  let oldListenerInput = listenerInput shIn
  let packageFlagsToAdd = map ExposePackage packageToImport
  let fileListenInfoToMerge = mempty { ignore = generateExcludePatterns ignoreModules } 
  let newCpIns = map ( \a -> mappend a $ mempty { cfDynFlags = addPackageToDynFlags packageFlagsToAdd } ) cpIns
  let newListFileListenInfo = map ( \ fli -> fli `mappend` fileListenInfoToMerge) (fileListenInfo oldListenerInput )
  let newListenerInput = oldListenerInput { fileListenInfo = newListFileListenInfo }
  return $ shIn {compileInputs = newCpIns, listenerInput = newListenerInput }
  where addPackageToDynFlags packageFlagToAdd dynFlags = dynFlags {
            packageFlags = packageFlags dynFlags ++ packageFlagToAdd
          } 
        currentPackage = localPkgDescr >>> package >>> pkgName >>> unPackageName $ lbi
        unPackageName (PackageName v) = v
        generateExcludePatterns :: [String] -> [String]
        generateExcludePatterns modList = map (\modName -> ".*" ++ modName ++ "\\.hs$" ) modList ++
                                          map (\modName -> ".*" ++ modName ++ "\\.lhs$" ) modList

