module Shaker.Cabal.CabalInputTest
 where 

import System.Directory
import Shaker.Action.Compile
import Shaker.Type
import Test.HUnit
import Control.Monad.Reader
import Control.Exception
import Shaker.Cabal.CabalInput


testCompileWithLocalSource :: Test
testCompileWithLocalSource = TestCase $ do
    runTestOnDirectory "testsuite/tests/resources/noSourceConfig" $ do
      shIn <- defaultCabalInput
      runReaderT runCompile shIn
      ex <- doesFileExist "target/hstest.o" 
      ex @? "file hsTest should exist"

testProjectCabalContentWithLocalSource :: Test
testProjectCabalContentWithLocalSource = TestCase $ do
  runTestOnDirectory "testsuite/tests/resources/noSourceConfig" $ do 
    shIn <- defaultCabalInput
    let (cplInp:_) = compileInputs shIn
    let (src:_) = cfSourceDirs cplInp
    src == "." @? "Expected . got " ++ src

runTestOnDirectory :: FilePath -> Assertion -> Assertion
runTestOnDirectory dir fun = do
  oldDir <- getCurrentDirectory 
  setCurrentDirectory dir
  res <- finally fun (setCurrentDirectory oldDir)
  return res

