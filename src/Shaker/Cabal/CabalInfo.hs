-- | Allow to use cabal configuration (generated via the configure action of cabal).
-- Source directories and compilation options will be reused by Shaker.
module Shaker.Cabal.CabalInfo
 where

import Shaker.Type
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
 
data CabalInfo = CabalInfo {
    sourceDir :: [String] -- ^ Location of hs sources
    ,cabalInfoDescripition :: String -- ^ Description of the artefact
    ,modules :: [String] -- ^ Exposed modules or main executable. It will be the target of the compilation.
    ,compileOption :: [String] -- ^ Options to pass to the compiler
    ,packagesToExpose :: [String] -- ^ List of package to expose 
  }
 deriving (Show)

-- * Default configuration
 
defaultCabalInfo :: CabalInfo
defaultCabalInfo = CabalInfo {
  sourceDir = ["."]
  ,cabalInfoDescripition = "Default cabalInfo"
  ,compileOption = ["-Wall"] 
  ,modules =[]
  ,packagesToExpose = []
  }

-- * Information extraction from cabal objects

-- | Try to get a library description from the localbuild info
-- and convert it to a CabalInfo
localBuildInfoToCabalInfoList :: LocalBuildInfo -> [CabalInfo]
localBuildInfoToCabalInfoList lbi = listArtifactToCabalInfo (library pkgDescription) (executables pkgDescription)
 where pkgDescription = localPkgDescr lbi
  
listArtifactToCabalInfo :: Maybe Library -> [Executable] -> [CabalInfo]
listArtifactToCabalInfo Nothing execs =  map executableToCabalInfo  execs
listArtifactToCabalInfo (Just lib) execs =libraryToCabalInfo lib : map executableToCabalInfo  execs 

executableToShakerInput :: Executable -> ShakerInput 
executableToShakerInput executable = 
  ShakerInput { 
     compileInputs = [cplInput]
  }
  where myExeBuildInfo = buildInfo executable
        mySourceDir = hsSourceDirs myExeBuildInfo
        cplInput = CompileInput {
          cfSourceDirs = mySourceDir
          ,cfDescription = "Executable : " ++ exeName executable
          ,cfTargetFiles = map (</> (modulePath executable)) mySourceDir
--          ,cfDynFlags = 
          ,cfCommandLineFlags = getCompileOptions myExeBuildInfo
        }

toDynFlags :: [String] -> [String] -> DynFlags -> DynFlags
toDynFlags sourceDirs packages dnFlags = dnFlags  {
    importPaths = sourceDirs
    ,outputFile = Just "target/Main"
    ,objectDir = Just "target"
    ,hiDir = Just "target"
    ,verbosity = 1  
    ,ghcLink = NoLink
    ,packageFlags = map ExposePackage $ packages
  } 

-- | Extract cabalInfo from an executable
executableToCabalInfo :: Executable -> CabalInfo
executableToCabalInfo executable = 
  CabalInfo {
    sourceDir = mySourceDir
    ,cabalInfoDescripition = "Executable : " ++ exeName executable
    ,compileOption = getCompileOptions myExeBuildInfo
    ,modules = map (</> (modulePath executable)) mySourceDir
    ,packagesToExpose = getLibDependencies myExeBuildInfo
  }
  where myExeBuildInfo = buildInfo executable
        mySourceDir = hsSourceDirs myExeBuildInfo

-- | Extract cabalInfo from a library
libraryToCabalInfo  :: Library -> CabalInfo
libraryToCabalInfo lib = 
  CabalInfo {
     sourceDir = hsSourceDirs libraryBuildInfo 
     ,cabalInfoDescripition = "Library : " ++ show myModules
     ,compileOption = getCompileOptions libraryBuildInfo 
     ,modules = myModules
     ,packagesToExpose = getLibDependencies libraryBuildInfo
  }
  where libraryBuildInfo = libBuildInfo lib
        myModules = map convertModuleNameToString $ exposedModules lib

-- * Helper methods

convertModuleNameToString :: ModuleName -> String
convertModuleNameToString modName  
  | null modArr = ""
  | otherwise =  foldr1 (\w s -> w ++ '.':s)  modArr
  where modArr = components modName 

getCompileOptions :: BuildInfo -> [String]
getCompileOptions myLibBuildInfo = ghcOptions ++ ghcExtensions 
  where ghcOptions = fromMaybe [] $ lookup GHC (options myLibBuildInfo)
        ghcExtensions = map (\a -> "-X"++ show a) (extensions myLibBuildInfo)

getLibDependencies :: BuildInfo -> [String] 
getLibDependencies bi = map getPackageName $ targetBuildDepends bi 

getPackageName :: Dependency -> String
getPackageName (Dependency (PackageName pn) _) = pn

-- | Check and filter all invalid main definission
checkCababInfoListForExecutables :: [CabalInfo] -> IO ([CabalInfo])
checkCababInfoListForExecutables = mapM checkCababInfoForExecutables 

checkCababInfoForExecutables :: CabalInfo -> IO (CabalInfo)
checkCababInfoForExecutables cabInf 
 | any (".hs" `isSuffixOf`) oldModules = do 
    newModules <- filterM doesFileExist oldModules  
    return cabInf {modules = newModules}
 | otherwise = return cabInf
 where oldModules = modules cabInf

