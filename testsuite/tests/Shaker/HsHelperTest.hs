module Shaker.HsHelperTest
 where

import Shaker.Type
import Shaker.HsHelper

import Test.HUnit

import Data.Monoid 

import Language.Haskell.Exts.Syntax

testModuleCollectProperties :: Assertion
testModuleCollectProperties = do
  parseMod <- getParsedModule
  let propertiesCollected = hsModuleCollectProperties parseMod
  propertiesCollected == ["prop_trivial"] @? "Should have properties, got " ++ show propertiesCollected

testModuleCollectAssertions :: Assertion
testModuleCollectAssertions = do
  parseMod <- getParsedModule
  let assertionsCollected = hsModuleCollectAssertions parseMod
  "testModuleCollectAssertions" `elem` assertionsCollected @? "Should have tests, got " ++ show assertionsCollected

testModuleCollectTestCases :: Assertion
testModuleCollectTestCases = do
  parseMod <- getParsedModule
  let testCollected = hsModuleCollectTest parseMod
  testCollected == ["testModuleCollectTestCase"] @? show testCollected

testModuleCollectTestCase :: Test
testModuleCollectTestCase = TestCase $ True @? "Trivial"

prop_trivial :: Bool
prop_trivial = True

getParsedModule :: IO Module 
getParsedModule = fmap head ( parseHsFiles [ mempty { fileListenInfoDir = "testsuite/tests/Shaker", fileListenInfoInclude = [".*HsHelperTest.hs"]  } ] )

