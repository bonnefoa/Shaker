module Shaker.ModuleDataTest
 where 

import Shaker.Type
import Shaker.ModuleData

import Test.HUnit

import System.FilePath
import System.Directory

import Language.Haskell.Exts.Syntax

import Data.Monoid 
import Data.List

import Shaker.CommonTest
import Control.Monad.Reader

testWriteModuleData :: Assertion
testWriteModuleData = do
  modData <- getParsedModule
  shIn <- testShakerInput
  runReaderT (writeModuleData modData) shIn
  doesFileExist "dist/shakerTarget/testsuite/tests/Shaker/ModuleDataTest.mdata" @? "module data file should exit"

testModuleDataFileName :: Assertion
testModuleDataFileName = do
  modData <- getParsedModule
  "ModuleDataTest.hs" `isSuffixOf` moduleDataFileName modData @? show modData

testModuleHasMain :: Assertion
testModuleHasMain = do
  (parsedMod:_) <- parseModuleData [ mempty {fileListenInfoDir ="prog" } ]
  moduleDataHasMain parsedMod @? "Should have main, got " ++ show parsedMod

testGroupByMethodUniqueGroup :: Assertion
testGroupByMethodUniqueGroup = do
  parsedMod <- parseModuleData [ mempty {fileListenInfoDir ="testsuite/tests/resources/sameFileInDifferentsModules" } ]
  let res = groupByValidTargets parsedMod 
  length res == 1 @? "Should have one group, got " ++ show res

testGroupByMethodMultipleGroups :: Assertion
testGroupByMethodMultipleGroups = do
  parsedMod <- parseModuleData [ mempty {fileListenInfoDir ="testsuite/tests/resources/" } ]
  let res = groupByValidTargets parsedMod 
  let filtered = map ( filter (\a -> "A" == moduleDataName a  ) ) res
  all (\a -> length a <= 1) filtered @? "Should have no group with more than one A module, got " ++ show filtered

testModuleDataHasTests :: Assertion
testModuleDataHasTests = do 
  modData <- getParsedModule
  hsModuleDataHasTest modData @? show modData

getParsedModule :: IO ModuleData 
getParsedModule = fmap head ( parseModuleData [ mempty { fileListenInfoDir = "testsuite/tests/Shaker", fileListenInfoInclude = [".*ModuleDataTest.hs"]  } ] )

