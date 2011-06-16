module Shaker.Action.Test
 where

import Data.Monoid
import Shaker.CabalInterface
import Shaker.Type
import Shaker.Reflexivite
import Shaker.ModuleData
import Shaker.TestTH
import Shaker.GhcInterface
import Control.Monad.Reader
import Language.Haskell.TH

runTestFramework :: Plugin
runTestFramework = applyPreprocessSources
  >> getModulesWithFunctionFiltering
  >>= fillModuleDataTest
  >>= processModuleDataList

runModuleTestFramework :: Plugin
runModuleTestFramework = applyPreprocessSources
  >> getModulesWithModuleFiltering
  >>= fillModuleDataTest
  >>= processModuleDataList

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

processModuleDataList :: [[ModuleData]] -> Plugin
processModuleDataList [] = lift $ putStrLn "No test to run"
processModuleDataList lst = mapM_ executeTest lst

executeTest :: [ModuleData] -> Plugin
executeTest moduleDatas = do
  let import_modules = base_modules ++ map moduleDataName moduleDatas
  resolvedExp <- lift $ runQ (listTestFrameworkGroupList moduleDatas)
  let function =  filter (/= '\n') $ pprint resolvedExp
  lift $ putStrLn function
  baseCpIn <- fmap mconcat (asks shakerCompileInputs)
  let cpIn = baseCpIn {
    compileInputTargetFiles = map moduleDataFileName moduleDatas
  }
  runFunction cpIn $ RunnableFunction import_modules listTestLibs ("defaultMain $ " ++ function)
  return ()
  where base_modules =["Data.Maybe","Shaker.TestHelper","Test.Framework", "Test.Framework.Providers.HUnit", "Test.Framework.Providers.QuickCheck2", "Test.QuickCheck", "Test.HUnit", "Prelude" ]

