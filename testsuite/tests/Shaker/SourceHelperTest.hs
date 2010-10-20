module Shaker.SourceHelperTest
 where

import Test.HUnit
import Shaker.SourceHelper
import Shaker.Type
import Shaker.CommonTest
import Control.Monad.Reader

import GHC
import DynFlags
import Data.List
import Data.Monoid

testConstructCompileFileList :: Assertion
testConstructCompileFileList =  runTestOnDirectory "testsuite/tests/resources/cabalTest" $ do 
  fileList <- testShakerInput >>= runReaderT constructCompileFileList 
  any (\cpFl -> "Main.hs" `isSuffixOf` cfFp cpFl && cfHasMain cpFl) fileList @? "Should have one main file, got " ++ show fileList
  any (\cpFl -> "src/CabalTest.hs" `isSuffixOf` cfFp cpFl && (not . cfHasMain) cpFl) fileList @? "Should have one main file, got " ++ show fileList
  not ( any (\cpFl -> "cabalTest/CabalTest.hs" `isSuffixOf` cfFp cpFl ) fileList ) @? "Should have excluded duplicated file, got " ++ show fileList
  let list_compile_file_paths = filter (\cpFl -> "Paths_cabalTest.hs" `isSuffixOf` cfFp cpFl) fileList
  length list_compile_file_paths== 1 @? "Should have only one Paths_cabalTest, got " ++ show list_compile_file_paths

testMergeCompileInputs  :: Assertion
testMergeCompileInputs  = runTestOnDirectory "testsuite/tests/resources/cabalTest" $ do  
  shIn <- testShakerInput
  let cpIn = mergeCompileInputsSources (shakerCompileInputs  shIn)
  let packageList = packageFlags $ compileInputDynFlags cpIn defaultDynFlags
  all (`elem` packageList ) [ExposePackage "mtl",ExposePackage "bytestring"] @? "mtl and bytestring should be exposed package"

testIgnoreEmacsFile :: Assertion
testIgnoreEmacsFile = runTestOnDirectory "testsuite/tests/resources/tempEmacsFile" $ do
  fileList <- testShakerInput >>= runReaderT constructCompileFileList 
  not ( any (\cpFl -> ".#TempFile.hs" `isSuffixOf` cfFp cpFl) fileList ) @? "Should ignore all .# files, got " ++ show fileList

testConstructConductorCompileFileList :: Assertion
testConstructConductorCompileFileList =  do
  list <- testShakerInput >>= runReaderT constructCompileFileList 
  let (Just cpFile) = find (\a ->  "Conductor.hs" `isSuffixOf` cfFp a ) list
  not (cfHasMain cpFile) && not (cfHasTH cpFile) @? "Should have conductor in list, got " ++ show cpFile

testCompileFileListConstruction :: Assertion
testCompileFileListConstruction =  do
  cpIn <- testCompileInput 
  list <- testShakerInput >>= runReaderT constructCompileFileList 
  let newCpIn = runReader (fillCompileInputWithStandardTarget cpIn) list 
  any (\a -> "Conductor.hs" `isSuffixOf` a) (compileInputTargetFiles newCpIn) @?"Should have conductor in list, got " ++ show (compileInputTargetFiles newCpIn)
