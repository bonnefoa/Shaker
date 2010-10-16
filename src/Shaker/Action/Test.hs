module Shaker.Action.Test
 where

import Shaker.Type
import Shaker.Reflexivite
import Control.Monad.Reader
import Language.Haskell.TH

runTestFramework :: Plugin 
runTestFramework = collectAllModulesForTest >>= getModulesWithFunctionFiltering  >>= runTestFramework'

runIntelligentTestFramework :: Plugin
runIntelligentTestFramework = collectChangedModulesForTest >>= getModulesWithFunctionFiltering >>= runTestFramework'

runModuleTestFramework :: Plugin 
runModuleTestFramework = collectAllModulesForTest >>= getModulesWithModuleFiltering >>= runTestFramework' 

runModuleIntelligentTestFramework :: Plugin
runModuleIntelligentTestFramework = collectChangedModulesForTest >>= getModulesWithModuleFiltering >>= runTestFramework'

getModulesWithModuleFiltering :: [ModuleMapping] -> Shaker IO [ModuleMapping] 
getModulesWithModuleFiltering module_list = fmap process (asks argument)
  where process [] = module_list
        process list = concatMap (filterModulesWithPattern module_list) list

getModulesWithFunctionFiltering :: [ModuleMapping] -> Shaker IO [ModuleMapping] 
getModulesWithFunctionFiltering module_list = fmap 
  (removeNonTestModule . filterFunctionsWithPatterns module_list)
  (asks argument)
  
runTestFramework' :: [ModuleMapping] -> Plugin
runTestFramework' [] = lift $ putStrLn "No test to run"
runTestFramework' modules = do
  let import_modules = base_modules ++ map cfModuleName modules
  resolvedExp <- lift $ runQ (listTestFrameworkGroupList modules)
  let function =  filter (/= '\n') $ pprint resolvedExp
  lift $ putStrLn function
  runFunction $ RunnableFunction import_modules ("defaultMain $ " ++ function) 
  return () 
  where base_modules =["Data.Maybe","Shaker.TestHelper","Test.Framework", "Test.Framework.Providers.HUnit", "Test.Framework.Providers.QuickCheck2", "Test.QuickCheck", "Test.HUnit", "Prelude" ] 

