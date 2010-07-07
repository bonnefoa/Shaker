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
  compileInput = cabalCompileInput lbi,
  listenerInput = defaultListenerInput,
  pluginMap = defaultPluginMap,
  commandMap = defaultCommandMap
  }

cabalCompileInput :: LocalBuildInfo -> CompileInput
cabalCompileInput lbi = CompileInput (cabalCompileFlags lbi) "-Wall"

cabalCompileFlags :: LocalBuildInfo -> (DynFlags -> DynFlags)
cabalCompileFlags lbi = \a-> a  {
    importPaths = ["src/","testsuite/tests/"]
    ,verbosity = 1
    ,outputFile = Just "target/Main"
    ,objectDir = Just "target"
    ,hiDir = Just "target"
    ,ghcLink = NoLink
  } 

data CabalUsefulInfo = CabalUsefulInfo {
    sourceDir :: [String]
    ,compileOption :: [String]
  }

defaultCabalInfo :: CabalUsefulInfo
defaultCabalInfo = CabalUsefulInfo ["src"] ["-Wall"]

{-
getCabalInformation :: LocalBuildInfo -> CabalUsefulInfo
getCabalInformation lbi = 
 case library (localPkgDescr lbi) of
      Nothing -> defaultCabalInfo
      Just lib -> let localBuildInfo = buildInfo lib in
          CabalUsefulInfo {
            sourceDir = hsSourceDirs localBuildInfo
            ,compileOption = snd $ options localBuildInfo
          }
-}



