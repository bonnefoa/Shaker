module Shaker.SourceHelperTest
 where

import Test.HUnit
import Shaker.SourceHelper
import Shaker.Type
import Shaker.CommonTest
import Control.Monad.Reader(runReader)

import GHC
import DynFlags
import Data.List
import Data.Monoid

testConstructCompileFileList :: Assertion
testConstructCompileFileList =  runTestOnDirectory "testsuite/tests/resources/cabalTest" $ do 
  let cpIn = mempty {cfSourceDirs = ["dist/build/autogen","src", "."]}
  fileList <- constructCompileFileList cpIn 
  any (\cpFl -> "Main.hs" `isSuffixOf` cfFp cpFl && cfHasMain cpFl) fileList @? "Should have one main file, got " ++ show fileList
  any (\cpFl -> "src/CabalTest.hs" `isSuffixOf` cfFp cpFl && (not . cfHasMain) cpFl) fileList @? "Should have one main file, got " ++ show fileList
  not ( any (\cpFl -> "cabalTest/CabalTest.hs" `isSuffixOf` cfFp cpFl ) fileList ) @? "Should have excluded duplicated file, got " ++ show fileList
  let list_compile_file_paths = filter (\cpFl -> "Paths_cabalTest.hs" `isSuffixOf` cfFp cpFl) fileList
  length list_compile_file_paths== 1 @? "Should have only one Paths_cabalTest, got " ++ show list_compile_file_paths

testMergeCompileInputs :: Assertion
testMergeCompileInputs = runTestOnDirectory "testsuite/tests/resources/cabalTest" $ do  
  shIn <- testShakerInput
  let cpIn = mergeCompileInputsSources (compileInputs shIn)
  let packageList = packageFlags $ cfDynFlags cpIn defaultDynFlags
  all (`elem` packageList ) [ExposePackage "mtl",ExposePackage "bytestring"] @? "mtl and bytestring should be exposed package"

testIgnoreEmacsFile :: Assertion
testIgnoreEmacsFile = runTestOnDirectory "testsuite/tests/resources/tempEmacsFile" $ do
  cpIn <- testCompileInput
  fileList <- constructCompileFileList cpIn 
  not ( any (\cpFl -> ".#TempFile.hs" `isSuffixOf` cfFp cpFl) fileList ) @? "Should ignore all .# files, got " ++ show fileList

testConstructConductorCompileFileList :: Assertion
testConstructConductorCompileFileList =  do
  list <- constructCompileFileList defaultCompileInput 
  let (Just cpFile) = find (\a ->  "Conductor.hs" `isSuffixOf` cfFp a ) list
  not (cfHasMain cpFile) && not (cfHasTH cpFile) @? "Should have conductor in list, got " ++ show cpFile

testCompileFileListConstruction :: Assertion
testCompileFileListConstruction =  do
  cpIn <- testCompileInput 
  list <- constructCompileFileList cpIn
  let newCpIn = runReader (fillCompileInputWithStandardTarget cpIn) list 
  any (\a -> "Conductor.hs" `isSuffixOf` a) (cfTargetFiles newCpIn) @?"Should have conductor in list, got " ++ show (cfTargetFiles newCpIn)
