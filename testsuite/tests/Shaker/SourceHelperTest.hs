module Shaker.SourceHelperTest
 where

import Test.HUnit
import Shaker.SourceHelper
import Shaker.Type
import Shaker.CommonTest
import Data.List
import Control.Monad.Reader

testConstructCompileFileList :: Test
testConstructCompileFileList = TestCase $ runTestOnDirectory "testsuite/tests/resources/cabalTest" $ do 
  let cpIn = initializeEmptyCompileInput {cfSourceDirs = ["src"]}
  fileList <- constructCompileFileList cpIn 
  any (\cpFl -> "Main.hs" `isSuffixOf` cfFp cpFl && cfHasMain cpFl) fileList @? "Should have one main file, got " ++ show fileList
  any (\cpFl -> "CabalTest.hs" `isSuffixOf` cfFp cpFl && (not . cfHasMain) cpFl) fileList @? "Should have one main file, got " ++ show fileList

testConstructConductorCompileFileList :: Test
testConstructConductorCompileFileList = TestCase $ do
  list <- constructCompileFileList defaultCompileInput 
  let (Just cpFile) = find (\a ->  "Conductor.hs" `isSuffixOf` (cfFp a) ) list
  not (cfHasMain cpFile) && not (cfHasTH cpFile) @? "Should have conductor in list, got " ++ show cpFile

testCompileInputConstruction :: Test
testCompileInputConstruction = TestCase $ do
  list <- constructCompileFileList defaultCompileInput 
  let newCpIn = runReader (setAllHsFilesAsTargets defaultCompileInput >>= removeFileWithMain >>= removeFileWithTemplateHaskell ) list
  any (\a -> "Conductor.hs" `isSuffixOf` a) (cfTargetFiles newCpIn) @?"Should have conductor in list, got " ++ show (cfTargetFiles newCpIn)
  
