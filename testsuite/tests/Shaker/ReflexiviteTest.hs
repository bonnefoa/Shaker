module Shaker.ReflexiviteTest
 where

import Shaker.Reflexivite
import Test.HUnit
import Data.List
import Shaker.Config 
import Control.Monad.Reader
import Shaker.Type

testRunReflexivite ::Test
testRunReflexivite = TestCase $ do
  modMapLst <- runReaderT runReflexivite testInputShaker 
  length modMapLst > 1 @? "Should have more than one module, got : "++ show (length modMapLst)
  any ( \(ModuleMapping nm _ _) -> nm == "Shaker.Action.ReflexiviteTest") modMapLst @? 
    "Should have module Shaker.Action.ReflexiviteTest, got " ++ show modMapLst
  let (Just regexpModMap)  = find (\(ModuleMapping nm _ _) -> nm == "Shaker.RegexTest" ) modMapLst
  any (== "prop_filterListAll") (cfPropName regexpModMap) @? "Should contain regexp module with quickechck properties prop_filterListAll, got "
    ++ show (cfPropName regexpModMap)
  let (Just reflexiviteModMap)  = find (\(ModuleMapping nm _ _) -> nm == "Shaker.Action.ReflexiviteTest" ) modMapLst
  any (== "testRunReflexivite") (cfHunitName reflexiviteModMap) @? "Should contain reflexivite test module with hunit test testRunReflexivite, got "
    ++ show (cfHunitName reflexiviteModMap)
    
testInputShaker :: ShakerInput
testInputShaker = defaultInput {
  compileInputs = [defaultCompileInput {
       cfCommandLineFlags = ["-package ghc"]
     }]
}

