module Shaker.GhcInterfaceTest
 where

import Control.Monad.Reader hiding (liftIO)
import Data.List
import Data.Monoid
import Shaker.Config
import Shaker.GhcInterface
import Shaker.CommonTest
import Shaker.Type
import Test.HUnit

testListNeededPackages :: Assertion
testListNeededPackages = do
  let cpIn = mempty {compileInputCommandLineFlags = ["-hide-all-packages"]}
  let shIn = defaultInput { shakerCompileInputs = [cpIn]  }
  list_needed_imports <- runReaderT getListNeededPackages shIn
  any (isPrefixOf "haskeline") list_needed_imports @? show list_needed_imports

testListModuleData :: Assertion 
testListModuleData = do
  shIn <- testShakerInput 
  modData <- getTestModuleData "HsHelperTest.hs"
  mDatas <- runReaderT (fillModuleDataTest [modData]) shIn
  length mDatas == 1 @? show mDatas
  let hsHelperMdata = head mDatas
  "trivialAssertion" `elem` moduleDataAssertions hsHelperMdata @? show mDatas
  moduleDataTestCase hsHelperMdata == ["trivialTestCase"] @? show mDatas

