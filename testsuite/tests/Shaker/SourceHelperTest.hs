module Shaker.SourceHelperTest
 where

import Test.HUnit
import Shaker.SourceHelper
import Shaker.Type
import Shaker.CommonTest
import Data.List

testConstuctCompileFileList :: Test
testConstuctCompileFileList = TestCase $ runTestOnDirectory "testsuite/tests/resources/cabalTest" $ do 
  let cpIn = CompileInput {cfSourceDirs = ["src"]}
  fileList <- constructCompileFileList cpIn 
  any (\cpFl -> "Main.hs" `isSuffixOf` (cfFp cpFl) && cfHasMain cpFl) fileList @? "Should have one main file, got " ++ show fileList
  any (\cpFl -> "CabalTest.hs" `isSuffixOf` (cfFp cpFl) && (not . cfHasMain) cpFl) fileList @? "Should have one main file, got " ++ show fileList

