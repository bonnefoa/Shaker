module Shaker.ModuleData
 where

import Data.List 
import Data.Monoid
import Data.Maybe

import Control.Monad.Reader
import Control.Arrow

import Shaker.Type
import Shaker.HsHelper

import Language.Haskell.Exts.Syntax

parseModuleData :: [FileListenInfo] -> IO [ ModuleData ]
parseModuleData mods = fmap (map constructModuleData) (parseHsFiles mods)

convertModuleDataToFullCompileInput :: Shaker IO [CompileInput]
convertModuleDataToFullCompileInput = do 
  baseCpIn <- fmap mconcat (asks shakerCompileInputs)
  lstModuleDatas <- asks shakerModuleData
  let groupOfCompileModules = groupByValidTargets lstModuleDatas
  return $ map ( \ lstModules -> baseCpIn { compileInputTargetFiles = map moduleDataToFileName lstModules } ) groupOfCompileModules

groupByValidTargets :: [ModuleData] -> [ [ ModuleData] ] 
groupByValidTargets = partition hsModuleDataHasMain 
  >>> first (groupBy mainGroupPredicate)
  >>> second ( nub )
  >>> ( \ (a, b) -> b : a ) 
  -- >>> uncurry (++)
  where mainGroupPredicate _ _ = False

getMergedCompileInput :: Shaker IO CompileInput
getMergedCompileInput = fmap mconcat convertModuleDataToFullCompileInput

fillModuleData :: ShakerInput -> IO ShakerInput
fillModuleData shIn = do
  lstHsModules <- shakerListenerInput >>> listenerInputFiles >>> parseHsFiles $ shIn
  return shIn { shakerModuleData = map constructModuleData lstHsModules }

constructModuleData :: Module -> ModuleData
constructModuleData hsModule = ModuleData {
  moduleDataName = hsModuleName hsModule
  ,moduleDataModule = hsModule
  ,moduleDataProperties = hsModuleCollectProperties hsModule
  ,moduleDataAssertions = hsModuleCollectAssertions hsModule
  ,moduleDataTestCase = hsModuleCollectTest hsModule
 }

moduleDataToFileName :: ModuleData -> String
moduleDataToFileName = moduleDataModule >>> hsModuleFileName

hsModuleDataHasMain :: ModuleData -> Bool
hsModuleDataHasMain = moduleDataModule >>> getTupleIdentType >>> map fst >>> any (=="main")

hsModuleDataHasTest :: ModuleData -> Bool
hsModuleDataHasTest hsModuleData = any (not . null) [moduleDataProperties hsModuleData, moduleDataAssertions hsModuleData] 

