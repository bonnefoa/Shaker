module Shaker.Cabal
 where

import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Shaker.Type
import Shaker.Config
import DynFlags

readConf :: IO (LocalBuildInfo)
readConf = getPersistBuildConfig "dist"

cabalInput :: LocalBuildInfo -> ShakerInput 
cabalInput lbi= ShakerInput {
  compileInput = cabalCompileInput $ getCabalInformation lbi,
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

data CabalInfo = CabalInfo {
    sourceDir :: [String]
    ,compileOption :: [String]
  }
 deriving (Show)

defaultCabalInfo :: CabalInfo
defaultCabalInfo = CabalInfo ["src"] ["-Wall"]

getCabalInformation :: LocalBuildInfo -> CabalInfo
getCabalInformation lbi = 
 case library (localPkgDescr lbi) of
      Nothing -> defaultCabalInfo
      Just lib -> let myLibBuildInfo = libBuildInfo lib in
          CabalInfo {
            sourceDir = hsSourceDirs myLibBuildInfo
            ,compileOption = getCompileOptions myLibBuildInfo
          }

getCompileOptions :: BuildInfo -> [String]
getCompileOptions myLibBuildInfo = snd . head $ options myLibBuildInfo

