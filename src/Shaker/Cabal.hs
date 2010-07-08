module Shaker.Cabal(
  defaultCabalInput
)
 where

import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr)
import Distribution.PackageDescription(BuildInfo,targetBuildDepends,options,libBuildInfo,library,Library,hsSourceDirs,exposedModules)
import Distribution.Compiler(CompilerFlavor(GHC))
import Distribution.Package (Dependency(Dependency), PackageName(PackageName))
import Shaker.Type
import Shaker.Config
import DynFlags(
    DynFlags, verbosity, ghcLink, packageFlags, outputFile, hiDir, objectDir ,importPaths
    ,PackageFlag (ExposePackage)
    ,GhcLink (NoLink)
  )
import Control.Monad(liftM)

defaultCabalInput :: IO(ShakerInput)
defaultCabalInput = liftM cabalInput readConf 

cabalInput :: LocalBuildInfo -> ShakerInput 
cabalInput lbi = defaultInput { 
      compileInput = cabalInfoToCompileInput cabalInfo 
      ,listenerInput = cabalInfoToListenerInput cabalInfo
  }
  where cabalInfo = localBuildInfoToCabalInfo lbi 
  
cabalInfoToCompileInput :: CabalInfo -> CompileInput
cabalInfoToCompileInput cabInf = defaultCompileInput {
  cfSourceDirs = sourceDir cabInf
  ,cfDynFlags = cabalCompileFlags cabInf
  ,cfCommandLineFlags = compileOption cabInf
}

cabalInfoToListenerInput :: CabalInfo -> ListenerInput
cabalInfoToListenerInput cabInfo = defaultListenerInput {
        fileListenInfo = map (\a -> FileListenInfo a [] [".*\\.hs$"]) (sourceDir  cabInfo)
 } 

cabalCompileFlags :: CabalInfo -> (DynFlags -> DynFlags)
cabalCompileFlags cabInfo = \a-> a  {
    importPaths = sourceDir cabInfo
    ,outputFile = Just "target/Main"
    ,objectDir = Just "target"
    ,hiDir = Just "target"
    ,verbosity = 1  
    ,ghcLink = NoLink
    ,packageFlags = map ExposePackage $ packagesToExpose cabInfo
  } 

localBuildInfoToCabalInfo :: LocalBuildInfo -> CabalInfo
localBuildInfoToCabalInfo lbi = 
 case library (localPkgDescr lbi) of
      Nothing -> defaultCabalInfo
      Just lib -> libraryToCabalInfo lib

libraryToCabalInfo  :: Library -> CabalInfo
libraryToCabalInfo lib = 
  CabalInfo {
     sourceDir = hsSourceDirs localBuildInfo 
     ,compileOption = getCompileOptions localBuildInfo 
     ,modules = map show $ exposedModules lib
     ,packagesToExpose = getLibDependencies localBuildInfo
  }
  where localBuildInfo = libBuildInfo lib


getCompileOptions :: BuildInfo -> [String]
getCompileOptions myLibBuildInfo = 
  case lookup GHC (options myLibBuildInfo) of
       Nothing -> []
       Just res -> res 
 
defaultCabalInfo :: CabalInfo
defaultCabalInfo = CabalInfo ["src"] [] ["-Wall"] []

getLibDependencies :: BuildInfo -> [String] 
getLibDependencies bi = map getPackageName $ targetBuildDepends bi 

getPackageName :: Dependency -> String
getPackageName (Dependency (PackageName pn) _) = pn

readConf :: IO (LocalBuildInfo)
readConf = getPersistBuildConfig "dist"


  
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
