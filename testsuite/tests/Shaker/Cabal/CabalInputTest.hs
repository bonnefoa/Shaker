module Shaker.Cabal.CabalInputTest
 where 

import System.Directory
import System.FilePath
import Shaker.Action.Compile
import Test.HUnit
import Control.Monad.Reader
import Shaker.Cabal.CabalInput
import Distribution.Simple.Configure 

testProjectWithLocalSource :: Test
testProjectWithLocalSource = TestCase $ do
  let directory = "testsuite/tests/resources/noSourceConfig"
  lbi <- getPersistBuildConfig $ directory </> "dist"
  let shIn = cabalInfosToShakerInput $ localBuildInfoToCabalInfoList lbi
  setCurrentDirectory ""
  runReaderT runCompile shIn 
  assertBool ("") False
  

