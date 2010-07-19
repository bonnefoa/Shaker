module Shaker.Cabal.CabalInputTest
 where 

import System.Directory
import Shaker.Action.Compile
import Shaker.Action.Clean
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
      ex <- doesFileExist "target/Main.o" 
      runReaderT runClean shIn
      ex2 <- doesFileExist "target/Main.o" 
      ex && not ex2 @? "file main should exist and be cleaned"

testProjectCabalContentWithLocalSource :: Test
testProjectCabalContentWithLocalSource = TestCase $ do
    runTestOnDirectory "testsuite/tests/resources/noSourceConfig" $ do
    shIn <- defaultCabalInput
    let cplInps@(cplInp:_) = compileInputs shIn
    length cplInps == 1 @? "Should have one compile input, got "++ (show $ length cplInps)
    let targs = cfTargetFiles $ cplInp
    targs == ["./noHsSource.hs"] @? "Expected [\"./noHsSource.hs\"] got " ++ show cplInp

runTestOnDirectory :: FilePath -> Assertion -> Assertion
runTestOnDirectory dir fun = do
  oldDir <- getCurrentDirectory 
  setCurrentDirectory dir
  res <- finally fun (setCurrentDirectory oldDir)
  return res

