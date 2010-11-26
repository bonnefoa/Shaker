module Shaker.ReflexiviteTest
 where

import Control.Monad.Reader(runReaderT)
import Data.List
import Data.Maybe
import Language.Haskell.TH
import Shaker.CommonTest
import Shaker.Properties()
import Shaker.Reflexivite
import Shaker.ModuleData
import Shaker.TestTH
import Shaker.Type
import System.Directory
import Test.HUnit
import Test.QuickCheck 

testListAllTestFrameworkGroupList :: Assertion
testListAllTestFrameworkGroupList = do
  resolved_exp <- runQ . listAllTestFrameworkGroupList =<< testShakerInput
  let function =  filter (/= '\n') $ pprint resolved_exp
  "processToTestGroup \"Shaker." `isInfixOf` function @? "listAllTestFrameworkGroupList should have correctly setted hunit to test framework conversion, got " ++ show function

aFun :: String -> IO ()
aFun tempFp = do
  exist <- doesDirectoryExist tempFp
  proc exist tempFp            
  where proc True fp = removeDirectory fp >> createDirectory fp
        proc _ fp = createDirectory fp

testRunFunction :: Assertion
testRunFunction = templateTestRunFunction ["Shaker.ReflexiviteTest"]

testRunFunctionWithEmptyModule :: Assertion
testRunFunctionWithEmptyModule = templateTestRunFunction [] 

templateTestRunFunction :: [String] -> Assertion
templateTestRunFunction modules=  do 
  tempFp <- getTemporaryDirectory >>= \a -> return $ a++"/testSha"
  let run = RunnableFunction modules listTestLibs $ "aFun " ++ show tempFp
  runReaderT (getNonMainCompileInput >>= flip runFunction run) =<< testShakerInput 
  doesDirectoryExist tempFp @? "Directory /tmp/testSha should have been created"
  
testSearchInstalledPackageId :: Assertion
testSearchInstalledPackageId = do 
  shIn <- testShakerInput
  may_pkgId <- runReaderT (searchInstalledPackageId "shaker") shIn
  isJust may_pkgId @? show may_pkgId

prop_filterModMap_fileListenInfoInclude_all :: [ModuleData] -> Bool
prop_filterModMap_fileListenInfoInclude_all modMap = modMap == res
  where res = filterModulesWithPattern modMap ".*"
        
prop_filterModMap_fileListenInfoInclude_some :: [ModuleData] -> Property
prop_filterModMap_fileListenInfoInclude_some modMap = (not . null) modMap ==> head res == head modMap
  where module_name = (moduleDataName . head) modMap
        res = filterModulesWithPattern modMap module_name

