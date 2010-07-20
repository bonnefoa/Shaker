-- | Allow to use cabal configuration (generated via the configure action of cabal).
-- Source directories and compilation options will be reused by Shaker.
module Shaker.Cabal.CabalInput(
  defaultCabalInput
--  ,localBuildInfoToCabalInfoList
  ,readConf
--  ,cabalInfosToShakerInput 
)
 where

import Shaker.Cabal.CabalInfo
import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Shaker.Io(FileListenInfo(..),defaultHaskellPatterns,defaultExclude)
import Shaker.Type
import Shaker.Config
import DynFlags(
    DynFlags, verbosity, ghcLink, packageFlags, outputFile, hiDir, objectDir ,importPaths
    ,PackageFlag (ExposePackage)
    ,GhcLink (NoLink)
  )

-- * Default configuration

defaultCabalInput :: IO ShakerInput
defaultCabalInput = readConf >>= \lbi ->
--  checkCababInfoListForExecutables . localBuildInfoToCabalInfoList  >>=
  return $ localBuildInfoToShakerInput lbi

readConf :: IO LocalBuildInfo
readConf = getPersistBuildConfig "dist"

{-
-- * CabalInfo converters
cabalInfosToShakerInput :: [CabalInfo] -> ShakerInput
cabalInfosToShakerInput cabalInfoList = defaultInput { 
      compileInputs = map cabalInfoToCompileInput cabalInfoList
      ,listenerInput = cabalInfoListToListenerInput cabalInfoList
  }

{-
cabalInfoToCompileInput :: CabalInfo -> CompileInput
cabalInfoToCompileInput cabInf = defaultCompileInput {
  cfSourceDirs = sourceDir cabInf
  ,cfDescription = cabalInfoDescripition cabInf
  ,cfTargetFiles = modules cabInf
  ,cfDynFlags = cabalCompileFlags cabInf
  ,cfCommandLineFlags = compileOption cabInf
}
-}
cabalInfoListToListenerInput :: [CabalInfo] -> ListenerInput
cabalInfoListToListenerInput  cabInfoList = defaultListenerInput {
        fileListenInfo = map (\a -> FileListenInfo a defaultExclude  defaultHaskellPatterns) concatSources
 } 
 where concatSources = concat $ map sourceDir cabInfoList

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
-}
