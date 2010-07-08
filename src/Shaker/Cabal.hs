module Shaker.Cabal
 where

import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Compiler
import Shaker.Type
import Shaker.Config
import DynFlags
import Control.Monad

defaultCabalInput :: IO(ShakerInput)
defaultCabalInput = liftM cabalInput readConf 

cabalInput :: LocalBuildInfo -> ShakerInput 
cabalInput lbi = ShakerInput {
  compileInput = cabalCompileInput $ getCabalLibInformation lbi,
  listenerInput = defaultListenerInput,
  pluginMap = defaultPluginMap,
  commandMap = defaultCommandMap
  }

cabalCompileInput :: CabalInfo -> CompileInput
cabalCompileInput cabInf = CompileInput (cabalCompileFlags cabInf)  $ compileOption cabInf

cabalCompileFlags :: CabalInfo -> (DynFlags -> DynFlags)
cabalCompileFlags cabInfo = \a-> a  {
    importPaths = sourceDir cabInfo
    ,outputFile = Just "target/Main"
    ,objectDir = Just "target"
    ,hiDir = Just "target"
    ,ghcLink = NoLink
  } 

getCabalLibInformation :: LocalBuildInfo -> CabalInfo
getCabalLibInformation lbi = 
 case library (localPkgDescr lbi) of
      Nothing -> defaultCabalInfo
      Just lib -> getCabalLibInformation' lib

getCabalLibInformation' :: Library -> CabalInfo
getCabalLibInformation' lib = 
  CabalInfo {
     sourceDir = hsSourceDirs myLibBuildInfo
     ,compileOption = getCompileOptions myLibBuildInfo
     ,modules = map show $ exposedModules lib
     ,packageType = LibraryType
  }
  where myLibBuildInfo = libBuildInfo lib

getCabalExecutableInformation :: LocalBuildInfo -> [CabalInfo]
getCabalExecutableInformation lbi = 
 map getCabalExecutableInformation' $ executables (localPkgDescr lbi) 

getCabalExecutableInformation' :: Executable -> CabalInfo
getCabalExecutableInformation' executable = 
  CabalInfo {
    sourceDir = hsSourceDirs myExeBuildInfo
    ,compileOption = getCompileOptions myExeBuildInfo
    ,modules = [modulePath executable]
    ,packageType = ExecutableType
  }
  where myExeBuildInfo = buildInfo executable

getCompileOptions :: BuildInfo -> [String]
getCompileOptions myLibBuildInfo = 
  case lookup GHC (options myLibBuildInfo) of
       Nothing -> []
       Just res -> res 
 
defaultCabalInfo :: CabalInfo
defaultCabalInfo = CabalInfo ["src"] [] ["-Wall"] LibraryType []

readConf :: IO (LocalBuildInfo)
readConf = getPersistBuildConfig "dist"

