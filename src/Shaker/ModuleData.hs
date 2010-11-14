module Shaker.ModuleData
 where

import Data.List 
import Data.Monoid

import Control.Monad.Reader
import Control.Arrow

import Shaker.Type
import Shaker.HsHelper
import Shaker.Io

import System.FilePath
import System.Directory
import Language.Haskell.Exts.Syntax

writeModuleData :: ModuleData -> Shaker IO ()
writeModuleData moduleData = do
  let srcFile = moduleDataFileName moduleData
  buildFile <- fmap (`addExtension` moduleDataExtension) (getCorrespondingBuildFile srcFile)
  lift $ createDirectoryIfMissing True (dropFileName buildFile) 
  lift $ writeFile buildFile (show moduleData) 

readModuleDataIfExist :: FilePath -> Shaker IO (Maybe ModuleData)
readModuleDataIfExist srcFile = do
  buildFile <- fmap (`addExtension` moduleDataExtension) (getCorrespondingBuildFile srcFile)
  exist <- lift $ doesFileExist buildFile
  if exist 
    then lift $ fmap Just (parseFileToModuleData buildFile)
    else return Nothing

parseFileToModuleData :: FilePath -> IO ModuleData
parseFileToModuleData src = fmap read (readFile src)

parseModuleData :: [FileListenInfo] -> IO [ ModuleData ]
parseModuleData mods = fmap (map constructModuleData) (parseHsFiles mods)

convertModuleDataToFullCompileInput :: Shaker IO [CompileInput]
convertModuleDataToFullCompileInput = do 
  baseCpIn <- fmap mconcat (asks shakerCompileInputs)
  lstModuleDatas <- asks shakerModuleData
  let groupOfCompileModules = groupByValidTargets lstModuleDatas
  return $ map ( \ lstModules -> baseCpIn { compileInputTargetFiles = map moduleDataFileName lstModules } ) groupOfCompileModules

groupByValidTargets :: [ModuleData] -> [ [ ModuleData] ] 
groupByValidTargets = partition moduleDataHasMain 
  >>> first (groupBy mainGroupPredicate)
  >>> second nub 
  >>> ( \ (a, b) -> b : a ) 
  where mainGroupPredicate _ _ = False

getNonMainCompileInput :: Shaker IO CompileInput
getNonMainCompileInput = do
  baseCpIn <- fmap mconcat (asks shakerCompileInputs)
  lstModuleDatas <- asks shakerModuleData
  let filteredModuleDatas = filter (not . moduleDataHasMain) >>> nub $ lstModuleDatas
  return $ baseCpIn { compileInputTargetFiles = map moduleDataFileName filteredModuleDatas } 

fillModuleData :: ShakerInput -> IO ShakerInput
fillModuleData shIn = do
  lstHsModules <- shakerListenerInput >>> listenerInputFiles >>> parseHsFiles $ shIn
  return shIn { shakerModuleData = map constructModuleData lstHsModules }

constructModuleData :: Module -> ModuleData
constructModuleData hsModule = ModuleData {
  moduleDataName = hsModuleName >>> unwrapModuleName $ hsModule
  ,moduleDataFileName = hsModuleFileName hsModule
  ,moduleDataHasMain = getTupleIdentType >>> map fst >>> any (=="main") $ hsModule
  ,moduleDataProperties = hsModuleCollectProperties hsModule
  ,moduleDataAssertions = hsModuleCollectAssertions hsModule
  ,moduleDataTestCase = hsModuleCollectTest hsModule
 }
 where unwrapModuleName (ModuleName a) = a

hsModuleDataHasTest :: ModuleData -> Bool
hsModuleDataHasTest hsModuleData = any (not . null) [moduleDataProperties hsModuleData, moduleDataAssertions hsModuleData] 

