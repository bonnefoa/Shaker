module Shaker.ModuleData
 where

import Data.List 
import Data.Monoid

import Control.Monad.Reader
import Control.Arrow

import Shaker.Type
import Shaker.HsHelper

convertModuleDataToFullCompileInput :: Shaker IO [CompileInput]
convertModuleDataToFullCompileInput = do 
  baseCpIn <- fmap mconcat (asks shakerCompileInputs)
  lstModuleDatas <- asks shakerModuleData
  let ( mainModulesData, nonMainModulesData ) = partition hsModuleDataHasMain >>> first (map moduleDataToModuleName) >>> second (map moduleDataToModuleName) $ lstModuleDatas 
  let libraries = baseCpIn { compileInputTargetFiles = nonMainModulesData } 
  let executables = map ( \ moduleName -> baseCpIn { compileInputTargetFiles = [moduleName] } ) mainModulesData
  return $ libraries : executables

getMergedCompileInput :: Shaker IO CompileInput
getMergedCompileInput = fmap mconcat convertModuleDataToFullCompileInput

