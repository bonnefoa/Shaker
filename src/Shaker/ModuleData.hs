module Shaker.ModuleData
 where

import Data.List 
import Data.Monoid
import Data.Maybe

import Control.Monad.Reader
import Control.Arrow

import Shaker.Type
import Shaker.HsHelper
import Shaker.Io

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax

convertModuleDataToFullCompileInput :: Shaker IO [CompileInput]
convertModuleDataToFullCompileInput = do 
  baseCpIn <- fmap mconcat (asks shakerCompileInputs)
  lstModuleDatas <- asks shakerModuleData
  let groupOfCompileModules = groupBy groupCondition lstModuleDatas
  return $ map ( \ lstModules -> baseCpIn { compileInputTargetFiles = map moduleDataToFileName lstModules } ) groupOfCompileModules
  where groupCondition mod1 mod2 = not (hsModuleDataHasMain mod1) && not (hsModuleDataHasMain mod2)
                                    && (moduleDataToModuleName mod1 /= moduleDataToModuleName mod2)

getMergedCompileInput :: Shaker IO CompileInput
getMergedCompileInput = fmap mconcat convertModuleDataToFullCompileInput

fillModuleData :: ShakerInput -> IO ShakerInput
fillModuleData shIn = do
  lstHsModules <- shakerListenerInput >>> listenerInputFiles >>> parseHsFiles $ shIn
  return shIn { shakerModuleData = map constructModuleData lstHsModules }
constructModuleData :: Module -> ModuleData
constructModuleData hsModule = ModuleData {
  moduleDataModule = hsModule
  ,moduleDataProperties = hsModuleCollectProperties hsModule
  ,moduleDataAssertions = hsModuleCollectAssertions hsModule
  ,moduleDataTestCase = hsModuleCollectTest hsModule
 }

moduleDataToModuleName :: ModuleData -> String
moduleDataToModuleName = moduleDataModule >>> hsModuleName

moduleDataToFileName :: ModuleData -> String
moduleDataToFileName = moduleDataModule >>> hsModuleFileName

hsModuleDataHasMain :: ModuleData -> Bool
hsModuleDataHasMain = moduleDataModule >>> getTupleIdentType >>> map fst >>> any (=="main")

hsModuleDataHasTest :: ModuleData -> Bool
hsModuleDataHasTest hsModuleData = any (not . null) [moduleDataProperties hsModuleData, moduleDataAssertions hsModuleData] 

