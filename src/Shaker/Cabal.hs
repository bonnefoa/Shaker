module Shaker.Cabal
 where

import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Compiler
import Distribution.Package
import Shaker.Type
import Shaker.Config
import DynFlags
import Control.Monad

defaultCabalInput :: IO(ShakerInput)
defaultCabalInput = liftM cabalInput readConf 

cabalInput :: LocalBuildInfo -> ShakerInput 
cabalInput lbi = defaultInput { 
      compileInput = cabalInfoToCompileInput cabalInfo 
      ,listenerInput = cabalInfoToListenerInput cabalInfo
  }
  where cabalInfo = localBuildInfoToCabalInfo lbi 
  
cabalInfoToCompileInput :: CabalInfo -> CompileInput
cabalInfoToCompileInput cabInf = CompileInput (cabalCompileFlags cabInf)  $ compileOption cabInf

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
