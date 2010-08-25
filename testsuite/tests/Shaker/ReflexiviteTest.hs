module Shaker.ReflexiviteTest
 where

import Test.HUnit
import Data.List
import Control.Monad.Reader(runReaderT)
import Shaker.Reflexivite
import Shaker.Type
import Shaker.CommonTest

import System.Directory
import System.FilePath 

testRunReflexivite ::Test
testRunReflexivite = TestCase $ do
  modMapLst <- runReaderT runReflexivite testShakerInput
  length modMapLst > 1 @? "Should have more than one module, got : "++ show (length modMapLst)
  any ( \(ModuleMapping nm _ _) -> nm == "Shaker.ReflexiviteTest") modMapLst @? 
    "Should have module Shaker.ReflexiviteTest, got " ++ show modMapLst
  let (Just regexpModMap)  = find (\(ModuleMapping nm _ _) -> nm == "Shaker.RegexTest" ) modMapLst
  any (== "prop_filterListAll") (cfPropName regexpModMap) @? "Should contain regexp module with quickechck properties prop_filterListAll, got "
    ++ show (cfPropName regexpModMap)
  let (Just reflexiviteModMap)  = find (\(ModuleMapping nm _ _) -> nm == "Shaker.ReflexiviteTest" ) modMapLst
  any (== "testRunReflexivite") (cfHunitName reflexiviteModMap) @? "Should contain reflexivite test module with hunit test testRunReflexivite, got "
    ++ show (cfHunitName reflexiviteModMap)
  not ( any (== "testShakerInput") (cfHunitName reflexiviteModMap) ) @? "Should not contain function testShakerInput, got "
    ++ show (cfHunitName reflexiviteModMap)
  not (any (\a ->cfModuleName a == "Shaker.RunTestTH") modMapLst) @? "Should have excluded RunTestTH, got " ++ show modMapLst

aFun :: String -> IO ()
aFun tempFp = do
  exist <- doesDirectoryExist tempFp
  proc exist tempFp            
  where proc True fp = removeDirectory fp >> createDirectory fp
        proc _ fp = createDirectory fp

testRunFunction :: Test
testRunFunction = templateTestRunFunction ["Shaker.ReflexiviteTest"]

testRunFunctionWithEmptyModule :: Test
testRunFunctionWithEmptyModule = templateTestRunFunction [] 

templateTestRunFunction :: [String] -> Test 
templateTestRunFunction modules= TestCase $ do 
  tempFp <- getTemporaryDirectory >>= \a -> return $ a++"/testSha"
  let run = RunnableFunction modules $ "aFun " ++ show tempFp
  runReaderT (runFunction run) testShakerInput 
  doesDirectoryExist tempFp @? "Directory /tmp/testSha should have been created"
  
testCollectChangedModules :: Test
testCollectChangedModules = TestCase $ do
  (cpIn,_) <- compileProject
  exp_no_modules <- runReaderT collectChangedModules testShakerInput 
  length exp_no_modules == 0 @? "There should be no modules to recompile"
  -- Remove a target file 
  let target = cfCompileTarget cpIn </> "Shaker" </> "SourceHelperTest.hi"
  removeFile target
  exp_one_modules <- runReaderT collectChangedModules testShakerInput 
  length exp_one_modules == 1 @? "One module (SourceHelperTest) should need compilation"

