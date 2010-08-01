module Shaker.Action.ReflexiviteTest
 where

import Shaker.Action.Reflexivite
import Test.HUnit
import Shaker.Config 
import Control.Monad.Reader
import Shaker.Type

testGetModuleNames ::Test
testGetModuleNames = TestCase $ do
  modMapLst <- runReaderT runReflexivite testInputShaker 
  length modMapLst > 1 @? "Should have more than one module, got : "++ show (length modMapLst)
  any ( \(ModuleMapping nm _ _) -> nm == "Shaker.Action.ReflexiviteTest") modMapLst @? 
    "Should have module Shaker.Action.ReflexiviteTest, got " ++ show modMapLst

testInputShaker :: ShakerInput
testInputShaker = defaultInput {
  compileInputs = [defaultCompileInput {
       cfCommandLineFlags = ["-package ghc"]
     }]
}
  
