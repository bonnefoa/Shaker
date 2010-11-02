module Shaker.ModuleDataTest
 where 

import Shaker.Type
import Shaker.ModuleData
import Shaker.HsHelper

import Test.HUnit

import Data.Monoid 
import Data.List

import Language.Haskell.Exts.Syntax

testModuleDataFileName :: Assertion
testModuleDataFileName = do
  modData <- fmap constructModuleData getParsedModule
  "ModuleDataTest.hs" `isSuffixOf` moduleDataToFileName modData @? show modData

testModuleHasMain :: Assertion
testModuleHasMain = do
  (parsedMod:_) <- parseHsFiles [ mempty {fileListenInfoDir ="prog" } ]
  hsModuleDataHasMain (constructModuleData parsedMod) @? "Should have main, got " ++ show parsedMod

testModuleDataHasTests :: Assertion
testModuleDataHasTests = do 
  modData <- fmap constructModuleData getParsedModule
  hsModuleDataHasTest modData @? show modData

getParsedModule :: IO Module 
getParsedModule = fmap head ( parseHsFiles [ mempty { fileListenInfoDir = "testsuite/tests/Shaker", fileListenInfoInclude = [".*ModuleDataTest.hs"]  } ] )

