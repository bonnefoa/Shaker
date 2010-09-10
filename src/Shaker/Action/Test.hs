module Shaker.Action.Test
 where

import Shaker.Type
import Shaker.Reflexivite
import Control.Monad.Reader
import Language.Haskell.TH

-- | Discover all quickcheck properties in the project 
-- and execute them
runQuickCheck :: Plugin
runQuickCheck = collectAllModulesForTest >>= runQuickCheck'

-- | Discover all Hunit test in the project and execute them
runHUnit :: Plugin
runHUnit = collectAllModulesForTest >>= runHUnit'

runIntelligentHunit :: Plugin
runIntelligentHunit = collectChangedModulesForTest >>= runHUnit'

runIntelligentQuickCheck :: Plugin
runIntelligentQuickCheck = collectChangedModulesForTest >>= runQuickCheck'
 
runQuickCheck' :: [ModuleMapping] -> Plugin
runQuickCheck' modMap  
 | null filteredModMap = lift $ putStrLn "No tests to run"
 | otherwise = do
     resolvedExp <- lift $ runQ (listProperties filteredModMap)
     let function =  filter (/= '\n') $ pprint resolvedExp
     let fullFunction = "sequence_ " ++ function ++ " >> return () "
     lift $ putStrLn fullFunction
     runFunction $ RunnableFunction modules fullFunction  
 where filteredModMap = filter (not . null . cfPropName) modMap
       modules = ["Test.QuickCheck","Prelude" ] ++ map cfModuleName filteredModMap 

runHUnit' :: [ModuleMapping] -> Plugin
runHUnit' mod_map = do 
  arg <- asks argument
  let filtered_mod_with_hunit = filter (not . null . cfHunitName) mod_map  
  let filtered_mod = filterModulesWithPattern arg filtered_mod_with_hunit 
  let import_modules = ["Test.HUnit","Prelude" ] ++ map cfModuleName filtered_mod
  resolvedExp <- lift $ runQ (listHunit filtered_mod)
  let function =  filter (/= '\n') $ pprint resolvedExp
  lift $ putStrLn function
  runFunction $ RunnableFunction import_modules ("runTestTT  $ TestList $ " ++ function) 
