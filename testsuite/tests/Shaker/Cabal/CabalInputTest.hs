module Shaker.Cabal.CabalInputTest
 where 

import System.Directory
import Shaker.Action.Compile
import Shaker.Type
import Test.HUnit
import Control.Monad.Reader
import Shaker.Cabal.CabalInput

runAll :: [IO Counts]
runAll = map execTest 
  [ 
    testProjectWithLocalSource,
    testProjectCabalContentWithLocalSource
  ]

execTest :: Test -> IO(Counts)
execTest testCase = do
  oldDir <- getCurrentDirectory 
  setCurrentDirectory "testsuite/tests/resources/noSourceConfig"
  res <- runTestTT $ testCase 
  setCurrentDirectory oldDir
  return res

testProjectWithLocalSource :: Test
testProjectWithLocalSource = TestCase $ do
    False @? "gra"
    shIn <- defaultCabalInput
    runReaderT runCompile shIn

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
  res <- fun
  setCurrentDirectory oldDir
  return res

