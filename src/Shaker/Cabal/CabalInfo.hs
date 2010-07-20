-- | Allow to use cabal configuration (generated via the configure action of cabal).
-- Source directories and compilation options will be reused by Shaker.
module Shaker.Cabal.CabalInfo
 where

import Shaker.Type
import Shaker.Config
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr)
import Distribution.ModuleName
import Distribution.PackageDescription(
  BuildInfo,targetBuildDepends,options,libBuildInfo,library,Library,hsSourceDirs,exposedModules, extensions, 
  Executable,buildInfo, modulePath, executables, exeName
  )
import DynFlags(
    DynFlags, verbosity, ghcLink, packageFlags, outputFile, hiDir, objectDir ,importPaths
    ,PackageFlag (ExposePackage)
    ,GhcLink (NoLink)
  )
import System.FilePath          ( (</>))
import Distribution.Compiler(CompilerFlavor(GHC))
import Distribution.Package (Dependency(Dependency), PackageName(PackageName))
import Data.Maybe
import Control.Monad
import System.Directory (doesFileExist)
import Data.List (isSuffixOf)

localBuildInfoToShakerInput :: LocalBuildInfo -> ShakerInput
localBuildInfoToShakerInput lbi = defaultInput {
    compileInputs = localBuildInfoToCompileInputs lbi
  }

localBuildInfoToCompileInputs :: LocalBuildInfo -> [CompileInput]
localBuildInfoToCompileInputs lbi = executableAndLibToCompileInput (library pkgDescription) (executables pkgDescription)
 where pkgDescription = localPkgDescr lbi

executableAndLibToCompileInput :: (Maybe Library) -> [Executable] -> [CompileInput]
executableAndLibToCompileInput Nothing exes = 
  map executableToCompileInput exes
executableAndLibToCompileInput (Just lib) exes = 
  libraryToCompileInput lib : map executableToCompileInput exes

executableToCompileInput :: Executable -> CompileInput
executableToCompileInput executable = defaultCompileInput { 
  cfSourceDirs = mySourceDir
  ,cfDescription = "Executable : " ++ exeName executable
  ,cfCommandLineFlags = getCompileOptions bldInfo
  ,cfTargetFiles = map (</> (modulePath executable) ) mySourceDir
  ,cfDynFlags = toDynFlags mySourceDir (getLibDependencies bldInfo)
  }
  where bldInfo = buildInfo executable
        mySourceDir = hsSourceDirs bldInfo

libraryToCompileInput :: Library -> CompileInput
libraryToCompileInput lib = defaultCompileInput {
  cfSourceDirs = mySourceDir
  ,cfDescription = "Library : " ++ show myModules
  ,cfCommandLineFlags = getCompileOptions bldInfo
  ,cfTargetFiles = myModules
  ,cfDynFlags = toDynFlags mySourceDir (getLibDependencies bldInfo)
 }
 where bldInfo = libBuildInfo lib
       myModules = map convertModuleNameToString $ exposedModules lib
       mySourceDir = hsSourceDirs bldInfo

toDynFlags :: [String] -> [String] -> DynFlags -> DynFlags
toDynFlags sourceDirs packagesToExpose dnFlags = dnFlags {
  importPaths = sourceDirs
  ,outputFile = Just "target/Main"
  ,objectDir = Just "target"
  ,hiDir = Just "target"
  ,verbosity = 1
  ,ghcLink = NoLink
  ,packageFlags = map ExposePackage $ packagesToExpose 
  } 

getCompileOptions :: BuildInfo -> [String]
getCompileOptions myLibBuildInfo = ghcOptions ++ ghcExtensions
 where ghcOptions = fromMaybe [] $ lookup GHC (options myLibBuildInfo)
       ghcExtensions = map (\a -> "-X"++ show a) (extensions myLibBuildInfo)

getLibDependencies :: BuildInfo -> [String]
getLibDependencies bi = map getPackageName $ targetBuildDepends bi 

getPackageName :: Dependency -> String
getPackageName (Dependency (PackageName pn) _) = pn

convertModuleNameToString :: ModuleName -> String
convertModuleNameToString modName
 | null modArr = ""
 | otherwise = foldr1 (\w s -> w ++ '.':s) modArr
   where modArr = components modName 

