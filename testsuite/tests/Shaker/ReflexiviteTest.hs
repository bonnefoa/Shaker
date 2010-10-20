module Shaker.ReflexiviteTest
 where

import Test.HUnit
import Test.QuickCheck 

import Data.List

import Control.Monad.Reader(runReaderT)
import Shaker.Reflexivite
import Shaker.Type
import Shaker.CommonTest
import Shaker.Properties()

import System.Time
import System.Directory
import System.FilePath 
import Language.Haskell.TH

-- * Module mapping construction test

abstractTestModuleMapping :: ([ModuleMapping] -> Assertion) -> Assertion
abstractTestModuleMapping predicat = testShakerInput >>= runReaderT collectAllModulesForTest >>= predicat

testModuleMappingLength :: Assertion
testModuleMappingLength = abstractTestModuleMapping predicat
  where predicat modMapLst = length modMapLst > 1 @? "Should have more than one module, got : "++ show (length modMapLst) 

testModuleMappingContainReflexiviteTest :: Assertion
testModuleMappingContainReflexiviteTest = abstractTestModuleMapping predicat
  where predicat modMapLst = any ( \mm -> cfModuleName mm == "Shaker.ReflexiviteTest") modMapLst @?  "Should have module Shaker.ReflexiviteTest, got " ++ show modMapLst

testModuleMappingShouldContainTestWithMain :: Assertion
testModuleMappingShouldContainTestWithMain = abstractTestModuleMapping predicat
  where predicat modMapLst = not (any (\a ->cfModuleName a == "Shaker.TestWithMain") modMapLst) @? "Should have fileListenInfoIncluded TestWithMain, got " ++ show modMapLst

-- * Reflexivite module Test 

abstractModuleMappingReflexiviteTest :: (ModuleMapping -> Assertion) -> Assertion
abstractModuleMappingReflexiviteTest predicat = do
  modMapLst <- runReaderT collectAllModulesForTest =<< testShakerInput 
  let (Just reflexiviteModMap)  = find (\mm -> cfModuleName mm == "Shaker.ReflexiviteTest" ) modMapLst
  predicat reflexiviteModMap

testReflexiviteTestContainQuickcheckProperty :: Assertion
testReflexiviteTestContainQuickcheckProperty = abstractModuleMappingReflexiviteTest predicat
  where predicat reflexiviteModule = any (== "prop_filterModMap_fileListenInfoInclude_all") (cfPropName reflexiviteModule) 
          @? "ReflexiviteModule should contains quickechck properties prop_filterModMap_fileListenInfoInclude_all, got " ++ show (cfPropName reflexiviteModule)

testReflexiviteTestContainHunitAssertion :: Assertion
testReflexiviteTestContainHunitAssertion = abstractModuleMappingReflexiviteTest predicat
  where predicat reflexiviteModule = any (== "testReflexiviteTestContainHunitAssertion") (cfHunitAssertion reflexiviteModule) 
          @? "ReflexiviteModule should contain hunit assertion testReflexiviteTestContainHunitAssertion, got " ++ show (cfHunitAssertion reflexiviteModule)

testReflexiviteTestShouldContainTestCase :: Assertion
testReflexiviteTestShouldContainTestCase = abstractModuleMappingReflexiviteTest predicat
  where predicat reflexiviteModule = any (== "testHunitTestCaseDetection") (cfHunitTestCase reflexiviteModule) 
          @? "ReflexiviteModule should contain testCase testHunitTestCaseDetection, got " ++ show (cfHunitTestCase reflexiviteModule)
  
testHunitTestCaseDetection :: Test
testHunitTestCaseDetection = TestCase $ True @? "Trivial"

testListAllTestFrameworkGroupList :: Assertion
testListAllTestFrameworkGroupList = do
  resolved_exp <- runQ . listAllTestFrameworkGroupList =<< testShakerInput
  let function =  filter (/= '\n') $ pprint resolved_exp
  "processToTestGroup \"Shaker.ReflexiviteTest\" [(\"testHunitTestCaseDetection\"" `isInfixOf` function @? "listAllTestFrameworkGroupList should have correctly setted hunit to test framework conversion"

aFun :: String -> IO ()
aFun tempFp = do
  exist <- doesDirectoryExist tempFp
  proc exist tempFp            
  where proc True fp = removeDirectory fp >> createDirectory fp
        proc _ fp = createDirectory fp

testRunFunction :: Assertion
testRunFunction = templateTestRunFunction ["Shaker.ReflexiviteTest"]

testRunFunctionWithEmptyModule :: Assertion
testRunFunctionWithEmptyModule = templateTestRunFunction [] 

templateTestRunFunction :: [String] -> Assertion
templateTestRunFunction modules=  do 
  tempFp <- getTemporaryDirectory >>= \a -> return $ a++"/testSha"
  let run = RunnableFunction modules $ "aFun " ++ show tempFp
  runReaderT (runFunction run) =<< testShakerInput 
  doesDirectoryExist tempFp @? "Directory /tmp/testSha should have been created"
  
testCollectChangedModules :: Assertion
testCollectChangedModules =  do
  (cpIn,_) <- compileProject
  exp_no_modules <- runReaderT collectChangedModulesForTest =<< testShakerInput 
  length exp_no_modules == 0 @? "There should be no modules to recompile"
  -- Remove a target file 
  let target = compileInputBuildDirectory cpIn </> "Shaker" </> "SourceHelperTest.hi"
  removeFile target
  exp_one_modules <- runReaderT collectChangedModulesForTest =<< testShakerInput 
  length exp_one_modules == 1 @? "One module (SourceHelperTest) should need compilation"

testCollectChangedModulesForTestHunit:: Assertion
testCollectChangedModulesForTestHunit =  do
  (cpIn,_) <- compileProject
  let target = compileInputBuildDirectory cpIn </> "Shaker" </> "SourceHelperTest.hi"
  removeFile target
  exp_one_modules <- runReaderT collectChangedModulesForTest =<< testShakerInput 
  length exp_one_modules == 1 @? "One module should need compilation"
  let module_mapping = head exp_one_modules 
  cfModuleName module_mapping == "Shaker.SourceHelperTest" @? "module SourceHelperTest should need recompilation, got " ++ cfModuleName module_mapping 
  length (cfHunitAssertion module_mapping) >2  @? "module SourceHelperTest should have hunit test" 
  
testCollectChangedModulesForTestQuickCheck :: Assertion
testCollectChangedModulesForTestQuickCheck =  do
  (cpIn,_) <- compileProject
  let target = compileInputBuildDirectory cpIn </> "Shaker" </> "RegexTest.hi"
  removeFile target
  exp_one_modules <- runReaderT collectChangedModulesForTest =<< testShakerInput 
  let module_mapping = head exp_one_modules  
  length exp_one_modules == 1 @? "One module should need compilation"
  cfModuleName module_mapping == "Shaker.RegexTest" @? "module RegexTest should need recompilation, got " ++ cfModuleName module_mapping 
  length (cfPropName module_mapping) >2  @? "module RegexTest should have properties" 

testCollectChangedModulesWithModifiedFiles :: Assertion
testCollectChangedModulesWithModifiedFiles =  do
  (cpIn,_) <- compileProject
  let sources = map (</> "Shaker" </> "SourceHelperTest.hs") (compileInputSourceDirs cpIn)
  let modFileInfo = map (\a -> FileInfo a (TOD 0 0) ) sources
  shIn <- testShakerInput 
  exp_one_modules <- runReaderT collectChangedModulesForTest shIn {shakerModifiedInfoFiles = modFileInfo }
  length exp_one_modules == 1 @? "One module should need compilation"

prop_filterModMap_fileListenInfoInclude_all :: [ModuleMapping] -> Bool
prop_filterModMap_fileListenInfoInclude_all modMap = modMap == res
  where res = filterModulesWithPattern modMap ".*"
        
prop_filterModMap_fileListenInfoInclude_some :: [ModuleMapping] -> Property
prop_filterModMap_fileListenInfoInclude_some modMap = (not . null) modMap ==> head res == head modMap
  where module_name = (cfModuleName . head) modMap
        res = filterModulesWithPattern modMap module_name

