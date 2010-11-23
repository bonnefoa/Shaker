module Shaker.Action.Test
 where

import Data.Monoid
import Shaker.Type
import Shaker.Reflexivite
import Shaker.GhcInterface
import Shaker.ModuleData
import Control.Monad.Reader
import Language.Haskell.TH

runTestFramework :: Plugin 
runTestFramework = getModulesWithFunctionFiltering >>= fillModuleDataTest >>= runTestFramework'

runModuleTestFramework :: Plugin 
runModuleTestFramework = getModulesWithModuleFiltering >>= fillModuleDataTest >>= runTestFramework' 

getModulesWithModuleFiltering :: Shaker IO [ModuleData] 
getModulesWithModuleFiltering = do
  listModuleData <- asks shakerModuleData
  args <- asks shakerArgument
  return $ process listModuleData args
  where process module_list [] = module_list
        process module_list list = concatMap (filterModulesWithPattern module_list) list

getModulesWithFunctionFiltering :: Shaker IO [ModuleData] 
getModulesWithFunctionFiltering = do
  listModuleData <- asks shakerModuleData
  args <- asks shakerArgument
  return $ filterFunctionsWithPatterns listModuleData args
  
runTestFramework' :: [ModuleData] -> Plugin
runTestFramework' [] = lift $ putStrLn "No test to run"
runTestFramework' moduleDatas = do
  let import_modules = base_modules ++ map moduleDataName filtered_modules
  resolvedExp <- lift $ runQ (listTestFrameworkGroupList filtered_modules)
  let function =  filter (/= '\n') $ pprint resolvedExp
  lift $ putStrLn function
  baseCpIn <- fmap mconcat (asks shakerCompileInputs)
  let cpIn = baseCpIn {
    compileInputTargetFiles = "Shaker.TestHelper" : map moduleDataFileName filtered_modules
  }
  runFunction cpIn $ RunnableFunction import_modules listTestLibs ("defaultMain $ " ++ function) 
  return () 
  where base_modules =["Data.Maybe","Shaker.TestHelper","Test.Framework", "Test.Framework.Providers.HUnit", "Test.Framework.Providers.QuickCheck2", "Test.QuickCheck", "Test.HUnit", "Prelude" ] 
        filtered_modules = removeNonTestModules moduleDatas

