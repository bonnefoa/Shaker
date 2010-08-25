module Shaker.SourceHelperTest
 where

import Test.HUnit
import Shaker.SourceHelper
import Shaker.Type
import Shaker.CommonTest
import Control.Monad.Reader(runReader,runReaderT)

import System.Directory
import System.FilePath 
import GHC
import GHC.Paths
import Data.List
import HscTypes
import Digraph
import Data.Maybe

testConstructCompileFileList :: Test
testConstructCompileFileList = TestCase $ runTestOnDirectory "testsuite/tests/resources/cabalTest" $ do 
  let cpIn = initializeEmptyCompileInput {cfSourceDirs = ["src"]}
  fileList <- constructCompileFileList cpIn 
  any (\cpFl -> "Main.hs" `isSuffixOf` cfFp cpFl && cfHasMain cpFl) fileList @? "Should have one main file, got " ++ show fileList
  any (\cpFl -> "CabalTest.hs" `isSuffixOf` cfFp cpFl && (not . cfHasMain) cpFl) fileList @? "Should have one main file, got " ++ show fileList

testConstructConductorCompileFileList :: Test
testConstructConductorCompileFileList = TestCase $ do
  list <- constructCompileFileList defaultCompileInput 
  let (Just cpFile) = find (\a ->  "Conductor.hs" `isSuffixOf` cfFp a ) list
  not (cfHasMain cpFile) && not (cfHasTH cpFile) @? "Should have conductor in list, got " ++ show cpFile

testCompileInputConstruction :: Test
testCompileInputConstruction = TestCase $ do
  list <- constructCompileFileList defaultCompileInput 
  let newCpIn = runReader (fillCompileInputWithStandardTarget defaultCompileInput) list 
  any (\a -> "Conductor.hs" `isSuffixOf` a) (cfTargetFiles newCpIn) @?"Should have conductor in list, got " ++ show (cfTargetFiles newCpIn)

testCheckUnchangedSources :: Test
testCheckUnchangedSources = TestCase $ do
  let cpIn = head . compileInputs $ testShakerInput
  cfFlList <- constructCompileFileList cpIn
  mss <- runGhc (Just libdir) $ do 
            _ <- initializeGhc $ runReader (fillCompileInputWithStandardTarget cpIn) cfFlList
            depanal [] False
  let hsSrcs = map (fromJust . ml_hs_file . ms_location) mss
      partialSrc = tail hsSrcs
      mapOfModifiedFiles = filter (==False) (map (checkUnchangedSources partialSrc ) mss )   
  all (checkUnchangedSources  []) mss @? "checkUnchangedSources with no modified files should be true"
  not (all (checkUnchangedSources  hsSrcs) mss ) @? "checkUnchangedSources with all modified files should be false"

  let lengthModifiedFiles = length mapOfModifiedFiles 
      lengthPartialSrc = length partialSrc 
  lengthModifiedFiles == lengthPartialSrc @? "checkUnchangedSources should output only " ++ show lengthPartialSrc ++ " but got " ++ show lengthModifiedFiles

testModuleNeedCompilation :: Test
testModuleNeedCompilation = TestCase $ do 
  (cpIn, cfFlList) <- compileProject
  runGhc (Just libdir) $ do 
      _ <- initializeGhc $ runReader (fillCompileInputWithStandardTarget cpIn) cfFlList
      mss <- depanal [] False
      let sort_mss = topSortModuleGraph True mss Nothing
      mapRecompNeeded <- mapM (isModuleNeedCompilation []) (flattenSCCs sort_mss)
      liftIO $ all (==False) mapRecompNeeded @? "There should be no modules to recompile"

compileProject :: IO(CompileInput, [CompileFile])
compileProject = do
  let cpIn = head . compileInputs $ testShakerInput
  cfFlList <- constructCompileFileList cpIn
  _ <- runGhc (Just libdir) $ 
      ghcCompile $ runReader (fillCompileInputWithStandardTarget cpIn) cfFlList
  return (cpIn, cfFlList)

testCollectChangedModules :: Test
testCollectChangedModules = TestCase $ do
  (cpIn,_) <- compileProject
  exp_no_modules <- runReaderT collectChangedModules testShakerInput 
  length exp_no_modules == 0 @? "There should be no modules to recompile"
  -- Remove a target file 
  let target = cfCompileTarget cpIn </> "Shaker" </> "SourceHelperTest.hi"
  removeFile target

  exp_one_modules <- runReaderT collectChangedModules testShakerInput 
  length exp_one_modules == 1 @? "One module (SourceHelperTest) should need compilation"
  

