module Shaker.GhcInterfaceTest
 where

import Test.HUnit

import Shaker.GhcInterface
import Shaker.SourceHelper
import Shaker.Type
import Shaker.CommonTest

import Control.Monad.Reader hiding (liftIO)

import GHC
import GHC.Paths
import HscTypes
import Digraph

import Data.Maybe
import Data.Monoid

import System.FilePath 

testListNeededPackages :: Assertion
testListNeededPackages = do
  let cpIn = mempty {cfCommandLineFlags = ["-hide-all-packages"]}
  shIn <- fmap (\ a -> a { compileInputs = [cpIn]  }) testShakerInput
  (bad_modules, list) <- runReaderT getListNeededPackages shIn
  null bad_modules @? "there should be no bad modules " ++ show bad_modules
  any (== "bytestring") list @? show list

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

