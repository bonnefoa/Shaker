module Shaker.ReflexiviteTest
 where

import Data.Maybe
import Test.HUnit
import GHC
import GHC.Paths
import Data.List
import HscTypes
import Control.Monad.Reader(runReader,runReaderT)
import System.Directory
import Shaker.SourceHelper
import Shaker.Reflexivite
import Shaker.CommonTest
import Shaker.Type
import Digraph

testRunReflexivite ::Test
testRunReflexivite = TestCase $ do
  modMapLst <- runReaderT runReflexivite testShakerInput
  length modMapLst > 1 @? "Should have more than one module, got : "++ show (length modMapLst)
  any ( \(ModuleMapping nm _ _) -> nm == "Shaker.ReflexiviteTest") modMapLst @? 
    "Should have module Shaker.ReflexiviteTest, got " ++ show modMapLst
  let (Just regexpModMap)  = find (\(ModuleMapping nm _ _) -> nm == "Shaker.RegexTest" ) modMapLst
  any (== "prop_filterListAll") (cfPropName regexpModMap) @? "Should contain regexp module with quickechck properties prop_filterListAll, got "
    ++ show (cfPropName regexpModMap)
  let (Just reflexiviteModMap)  = find (\(ModuleMapping nm _ _) -> nm == "Shaker.ReflexiviteTest" ) modMapLst
  any (== "testRunReflexivite") (cfHunitName reflexiviteModMap) @? "Should contain reflexivite test module with hunit test testRunReflexivite, got "
    ++ show (cfHunitName reflexiviteModMap)
  not ( any (== "testShakerInput") (cfHunitName reflexiviteModMap) ) @? "Should not contain function testShakerInput, got "
    ++ show (cfHunitName reflexiviteModMap)
  not (any (\a ->cfModuleName a == "Shaker.RunTestTH") modMapLst) @? "Should have excluded RunTestTH, got " ++ show modMapLst

aFun :: String -> IO ()
aFun tempFp = do
  exist <- doesDirectoryExist tempFp
  proc exist tempFp            
  where proc True fp = removeDirectory fp >> createDirectory fp
        proc _ fp = createDirectory fp

testRunFunction :: Test
testRunFunction = templateTestRunFunction ["Shaker.ReflexiviteTest"]

testRunFunctionWithEmptyModule :: Test
testRunFunctionWithEmptyModule = templateTestRunFunction [] 

templateTestRunFunction :: [String] -> Test 
templateTestRunFunction modules= TestCase $ do 
  tempFp <- getTemporaryDirectory >>= \a -> return $ a++"/testSha"
  let run = RunnableFunction modules $ "aFun " ++ show tempFp
  runReaderT (runFunction run) testShakerInput 
  doesDirectoryExist tempFp @? "Directory /tmp/testSha should have been created"

testCheckUnchangedSources :: Test
testCheckUnchangedSources = TestCase $ do
  let cpIn = head . compileInputs $ testShakerInput
  cfFlList <- constructCompileFileList cpIn
  mss <- runGhc (Just libdir) $ do 
            _ <- initializeGhc $ runReader (setAllHsFilesAsTargets cpIn >>= removeFileWithMain >>=removeFileWithTemplateHaskell) cfFlList
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
  let cpIn = head . compileInputs $ testShakerInput
  cfFlList <- constructCompileFileList cpIn
  runGhc (Just libdir) $ do 
            _ <- initializeGhc $ runReader (setAllHsFilesAsTargets cpIn >>= removeFileWithMain >>=removeFileWithTemplateHaskell) cfFlList
            mss <- depanal [] False
            let sort_mss = topSortModuleGraph True mss Nothing
            mapRecompNeeded <- mapM (isModuleNeedCompilation []) (flattenSCCs sort_mss)
            liftIO $ all (==False) mapRecompNeeded @? "There should be no modules to recompile"

  

