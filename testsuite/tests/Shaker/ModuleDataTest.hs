module Shaker.ModuleDataTest
 where 

import Control.Monad.Reader
import Data.List
import Data.Maybe
import Shaker.CommonTest
import Shaker.ModuleData
import Shaker.Type
import System.FilePath
import Test.HUnit

getAllModuleData :: IO [ModuleData]
getAllModuleData = testShakerInput >>= runReaderT parseAllModuleData 

testReadWriteModuleData :: Assertion
testReadWriteModuleData = do
  shIn <- testShakerInput
  moduleData <- getTestModuleData "ModuleDataTest.hs"
  runReaderT (writeModuleData moduleData) shIn
  let testFile = testDirectory </> "ModuleDataTest.hs"
  parsedModule <- runReaderT (parseModuleDataIfExist testFile) shIn
  Just moduleData == parsedModule @? "Module datas should be equals, got " ++ show moduleData ++ " and " ++ show parsedModule
  
testGroupModuleData :: Assertion
testGroupModuleData = do 
  shIn <- testShakerInput
  parsedMod <- fmap groupByValidTargets (runReaderT parseAllModuleData shIn)
  let res = filter ( any ( \ md -> "/noHsSource.hs" `isSuffixOf` moduleDataFileName md) ) parsedMod
  length res == 1 @? show res
  
testNubModuleData :: Assertion
testNubModuleData = do
  parsedMod <- fmap nub getAllModuleData
  let res = filter ( \ md -> "/CabalTest.hs" `isSuffixOf` moduleDataFileName md) parsedMod
  length res == 1 @? show res

testGetNonMain :: Assertion
testGetNonMain = do
 shIn <- testShakerInput
 cpIn <- runReaderT getNonMainCompileInput shIn
 let filtered = filter (isSuffixOf "CabalTest.hs"  ) (compileInputTargetFiles cpIn )
 length filtered ==1 @? show filtered

testModuleDataFileName :: Assertion
testModuleDataFileName = do
  modData <- getTestModuleData "ModuleDataTest.hs"
  "ModuleDataTest.hs" `isSuffixOf` moduleDataFileName modData @? show modData

testModuleHasMain :: Assertion
testModuleHasMain = do
  parsedMod <- testShakerInput >>= runReaderT (parseModuleData "prog/Shaker.hs") >>= return . fromJust
  moduleDataHasMain parsedMod @? "Should have main, got " ++ show parsedMod

testModuleDataHasTests :: Assertion
testModuleDataHasTests = do 
  modData <- getTestModuleData "ModuleDataTest.hs"
  hsModuleDataHasTest modData @? show modData

testDirectory :: FilePath
testDirectory = "testsuite/tests/Shaker"

getTestModuleData :: String -> IO ModuleData
getTestModuleData testFile = 
  testShakerInput >>= runReaderT (parseModuleData $ testDirectory </> testFile) >>= return . fromJust

