module Shaker.HsHelperTest
 where

import Data.Monoid 
import Language.Haskell.Syntax
import Shaker.HsHelper
import Shaker.Type
import Test.HUnit

testModuleCollectProperties :: Assertion
testModuleCollectProperties = do
  parseMod <- getParsedModule
  let propertiesCollected = hsModuleCollectProperties parseMod
  propertiesCollected == ["prop_trivial"] @? "Should have properties, got " ++ show parseMod

testModuleCollectPropertiesBis :: Assertion
testModuleCollectPropertiesBis = do
  parseMod <- getTestModule "RegexTest.hs"
  let propertiesCollected = hsModuleCollectProperties parseMod
  "prop_filterListAll" `elem` propertiesCollected @? "Should have properties, got " ++ show parseMod

testModuleCollectAssertions :: Assertion
testModuleCollectAssertions = do
  parseMod <- getParsedModule
  let assertionsCollected = hsModuleCollectAssertions parseMod
  "testModuleCollectAssertions" `elem` assertionsCollected @? "Should have tests, got " ++ show assertionsCollected

testModuleCollectTestCases :: Assertion
testModuleCollectTestCases = do
  parseMod <- getParsedModule
  let testCollected = hsModuleCollectTest parseMod
  testCollected == ["testModuleCollectTestCase"] @? show parseMod

trivialTestCase = TestCase trivialAssertion

trivialAssertion = True @? "Trivial"

prop_trivial = True

getParsedModule :: IO HsModule 
getParsedModule = getTestModule "HsHelperTest.hs"

getTestModule :: String -> IO HsModule 
getTestModule file = fmap head ( parseHsFiles [ mempty { fileListenInfoDir = "testsuite/tests/Shaker", fileListenInfoInclude = [".*" ++ file]  } ] )

