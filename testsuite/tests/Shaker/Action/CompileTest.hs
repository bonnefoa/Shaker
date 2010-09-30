module Shaker.Action.CompileTest
 where

import Shaker.Action.Compile
import Test.HUnit
import System.Directory
import Control.Monad.Reader
import Shaker.CommonTest 
  
testRunCompileProject :: Assertion
testRunCompileProject = 
  runReaderT runCompile  testShakerInput >> 
  getDirectoryContents "target/Shaker" >>= \cont ->
  doesFileExist "target/Shaker/Action/CompileTest.o" >>= \ex ->
  doesFileExist "target/Shaker/Action/CompileTest.hi" >>= \ex2 ->
  assertBool ("File .o and hi should exists "++ show cont) (ex && ex2)

testRunFullCompile :: Assertion
testRunFullCompile = do
  runReaderT runFullCompile testShakerInput
  cont <- getDirectoryContents "target/Shaker" 
  ex <- doesFileExist "target/Shaker/Conductor.o" 
  assertBool ("Conductor.o should exist, got "++show cont) ex

