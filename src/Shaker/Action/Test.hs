module Shaker.Action.Test
 where

import Shaker.Type
import Shaker.Reflexivite
import Control.Monad.Trans
import Control.Monad.Reader
import Language.Haskell.TH

-- | Discover all quickcheck properties in the project 
-- and execute them
runQuickCheck :: Plugin
runQuickCheck = collectAllModulesForTest >>= runQuickCheck'

runIntelligentQuickCheck :: Plugin
runIntelligentQuickCheck = collectChangedModulesForTest >>= runQuickCheck'
 
runQuickCheck' :: [ModuleMapping] -> Plugin
runQuickCheck' modMap = do
  let filteredModMap = filter (not . null . cfPropName) modMap
  let modules = ["Test.QuickCheck","Prelude" ] ++ map cfModuleName filteredModMap 
  expression <- asks listProperties 
  resolvedExp <- lift $ runQ expression
  let function =  filter (/= '\n') $ pprint resolvedExp
  lift $ putStrLn function
  runFunction $ RunnableFunction modules ("sequence_ " ++ function ++ " >> return () ") 


-- | Discover all Hunit test in the project and execute them
runHUnit :: Plugin
runHUnit = do 
  modMap <- collectAllModulesForTest
  let filteredModMap = filter (not . null . cfHunitName) modMap
  let modules = ["Test.HUnit","Prelude" ] ++ map cfModuleName filteredModMap 
  expression <- asks listHunit
  resolvedExp <- lift $ runQ expression
  let function =  filter (/= '\n') $ pprint resolvedExp
  lift $ putStrLn function
  runFunction $ RunnableFunction modules ("runTestTT  $ TestList $ " ++ function) 

