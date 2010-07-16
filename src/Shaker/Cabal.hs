-- | Allow to use cabal configuration (generated via the configure action of cabal).
-- Source directories and compilation options will be reused by Shaker.
module Shaker.Cabal(
  defaultCabalInput
)
 where

import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr)
import Distribution.ModuleName
import Distribution.PackageDescription(BuildInfo,targetBuildDepends,options,libBuildInfo,library,Library,hsSourceDirs,exposedModules, extensions)
import Distribution.Compiler(CompilerFlavor(GHC))
import Distribution.Package (Dependency(Dependency), PackageName(PackageName))
import Shaker.Io(FileListenInfo(..))
import Shaker.Type
import Shaker.Config
import DynFlags(
    DynFlags, verbosity, ghcLink, packageFlags, outputFile, hiDir, objectDir ,importPaths
    ,PackageFlag (ExposePackage)
    ,GhcLink (NoLink)
  )
import Control.Monad(liftM)
import Data.Maybe

data CabalInfo = CabalInfo {
    sourceDir :: [String] -- ^ Location of hs sources
    ,modules :: [String] -- ^ Exposed modules or main executable. It will be the target of the compilation.
    ,compileOption :: [String] -- ^ Options to pass to the compiler
    ,packagesToExpose :: [String] -- ^ List of package to expose 
  }
 deriving (Show)

-- * Default configuration

defaultCabalInput :: IO ShakerInput
defaultCabalInput = liftM cabalInput readConf 
 
defaultCabalInfo :: CabalInfo
defaultCabalInfo = CabalInfo {
  sourceDir = ["."]
  ,compileOption = ["-Wall"] 
  ,modules =[]
  ,packagesToExpose = []
  }

readConf :: IO LocalBuildInfo
readConf = getPersistBuildConfig "dist"

-- | Convert a cabal localBuildInfo to a shakerInput
-- It try to obtain all necessary information like source dirs, 
-- library dependencies for the project compilation. 
cabalInput :: LocalBuildInfo -> ShakerInput 
cabalInput lbi = defaultInput { 
      compileInputs = [cabalInfoToCompileInput cabalInfo]
      ,listenerInput = cabalInfoToListenerInput cabalInfo
  }
  where cabalInfo = localBuildInfoToCabalInfo lbi 

-- * CabalInfo converters
  
cabalInfoToCompileInput :: CabalInfo -> CompileInput
cabalInfoToCompileInput cabInf = defaultCompileInput {
  cfSourceDirs = sourceDir cabInf
  ,cfTargetFiles = modules cabInf
  ,cfDynFlags = cabalCompileFlags cabInf
  ,cfCommandLineFlags = compileOption cabInf
}

cabalInfoToListenerInput :: CabalInfo -> ListenerInput
cabalInfoToListenerInput cabInfo = defaultListenerInput {
        fileListenInfo = map (\a -> FileListenInfo a [".*Setup\\.hs$"] defaultHaskellPatterns) (sourceDir  cabInfo)
 } 

cabalCompileFlags :: CabalInfo -> DynFlags -> DynFlags
cabalCompileFlags cabInfo dnFlags = dnFlags  {
    importPaths = sourceDir cabInfo
    ,outputFile = Just "target/Main"
    ,objectDir = Just "target"
    ,hiDir = Just "target"
    ,verbosity = 1  
    ,ghcLink = NoLink
    ,packageFlags = map ExposePackage $ packagesToExpose cabInfo
  } 

-- * Information extraction from cabal objects

-- | Try to get a library description from the localbuild info
-- and convert it to a CabalInfo
localBuildInfoToCabalInfo :: LocalBuildInfo -> CabalInfo
localBuildInfoToCabalInfo lbi = 
 case library (localPkgDescr lbi) of
      Nothing -> defaultCabalInfo
      Just lib -> libraryToCabalInfo lib 

libraryToCabalInfo  :: Library -> CabalInfo
libraryToCabalInfo lib = 
  CabalInfo {
     sourceDir = hsSourceDirs libraryBuildInfo 
     ,compileOption = getCompileOptions libraryBuildInfo 
     ,modules = map convertModuleNameToString $ exposedModules lib
     ,packagesToExpose = getLibDependencies libraryBuildInfo
  }
  where libraryBuildInfo = libBuildInfo lib

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
  
{-
executableToCabalInfo  :: Executable -> CabalInfo
executableToCabalInfo  executable = 
  CabalInfo {
    sourceDir = hsSourceDirs myExeBuildInfo
    ,compileOption = getCompileOptions myExeBuildInfo
    ,modules = [modulePath executable]
    ,packageType = ExecutableType
    ,packagesToExpose = getLibDependencies myExeBuildInfo
  }
  where myExeBuildInfo = buildInfo executable

--getCabalExecutableInformation :: LocalBuildInfo -> [CabalInfo]
--getCabalExecutableInformation lbi = 
-- map executableToCabalInfo $ executables (localPkgDescr lbi) 
-}
