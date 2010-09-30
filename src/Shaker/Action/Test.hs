module Shaker.Action.Test
 where

import Shaker.Type
import Shaker.Reflexivite
import Control.Monad.Reader
import Language.Haskell.TH

runTestFramework :: Plugin 
runTestFramework = collectAllModulesForTest  >>= runTestFramework'
        
runIntelligentTestFramework :: Plugin
runIntelligentTestFramework = collectChangedModulesForTest >>= runTestFramework'

runTestFramework' :: [ModuleMapping] -> Plugin
runTestFramework' modules = do
  arg <- asks argument 
  let filtered_mod = (filterModulesWithPattern arg . filter (\modMap -> notEmpty (cfHunitName modMap) || notEmpty (cfPropName modMap) )) modules 
  let import_modules = base_modules ++ map cfModuleName filtered_mod
  resolvedExp <- lift $ runQ (listTestFrameworkGroupList filtered_mod)
  let function =  filter (/= '\n') $ pprint resolvedExp
  lift $ putStrLn function
  runFunction $ RunnableFunction import_modules ("defaultMain $ " ++ function) 
  return () 
  where notEmpty = not.null
        base_modules =["Test.Framework", "Test.Framework.Providers.HUnit", "Test.Framework.Providers.QuickCheck2", "Test.QuickCheck", "Test.HUnit", "Prelude" ] 

