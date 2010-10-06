module Shaker.SourceHelperTest
 where

import Test.HUnit
import Shaker.SourceHelper
import Shaker.Type
import Shaker.CommonTest
import Control.Monad.Reader(runReader, filterM)

import GHC
import GHC.Paths
import DynFlags
import Data.List
import HscTypes
import Digraph
import Data.Maybe

import System.FilePath 

testConstructCompileFileList :: Assertion
testConstructCompileFileList =  runTestOnDirectory "testsuite/tests/resources/cabalTest" $ do 
  let cpIn = initializeEmptyCompileInput {cfSourceDirs = ["dist/build/autogen","src", "."]}
  fileList <- constructCompileFileList cpIn 
  any (\cpFl -> "Main.hs" `isSuffixOf` cfFp cpFl && cfHasMain cpFl) fileList @? "Should have one main file, got " ++ show fileList
  any (\cpFl -> "CabalTest.hs" `isSuffixOf` cfFp cpFl && (not . cfHasMain) cpFl) fileList @? "Should have one main file, got " ++ show fileList
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

testCheckUnchangedSources :: Assertion
testCheckUnchangedSources =  do
  cpIn <- testCompileInput
  cfFlList <-  constructCompileFileList cpIn
  mss <- runGhc (Just libdir) $ do 
            _ <- initializeGhc $ runReader (fillCompileInputWithStandardTarget cpIn) cfFlList
            depanal [] False
  let hsSrcs = map (fromJust . ml_hs_file . ms_location) mss
  exp_all_true <- filterM (checkUnchangedSources  []) mss 
  exp_all_false <- filterM (checkUnchangedSources hsSrcs) mss 
  exp_one_true <- filterM ( checkUnchangedSources (tail hsSrcs) ) mss 
  exp_one_false <- filterM ( checkUnchangedSources [head hsSrcs] ) mss 
  length exp_all_true == length hsSrcs @? "checkUnchangedSources with no modified files should be true"
  length exp_all_false == 0 @? "checkUnchangedSources with all modified files should be false" 
  length exp_one_true == 1 @? "partial checkUnchangedSources should have only one true, got " ++ show (length exp_one_true)
  length exp_one_false == length hsSrcs - 1 @? "partial checkUnchangedSources should have only one false"

testModuleNeedCompilation :: Assertion
testModuleNeedCompilation =  do 
 (cpIn, cfFlList) <- compileProject
 let targets = map (</> "Shaker" </> "SourceHelperTest.hs") (cfSourceDirs cpIn)
 runGhc (Just libdir) $ do 
     _ <- initializeGhc $ runReader (fillCompileInputWithStandardTarget cpIn) cfFlList
     mss <- depanal [] False
     let sort_mss = topSortModuleGraph True mss Nothing
     mapRecompNeeded <- mapM (isModuleNeedCompilation []) (flattenSCCs sort_mss)
     liftIO $ all (==False) mapRecompNeeded @? "There should be no modules to recompile"
     exp_one_mapRecompNeeded <- mapM (isModuleNeedCompilation targets) (flattenSCCs sort_mss)
     liftIO $ any (==True) exp_one_mapRecompNeeded @? "There should be at least on module to recompile"

