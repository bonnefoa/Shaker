module Shaker.Action.CompileTest
 where

import Shaker.Action.Compile
import Test.HUnit
import System.Directory
import Shaker.Config 
  
testRunCompileProject :: Test
testRunCompileProject = TestCase $ 
  runCompile defaultInput >> 
  getDirectoryContents "target/Shaker" >>= \cont ->
  doesFileExist "target/Shaker.Action.CompileTest.o" >>= \ex ->
  doesFileExist "target/Shaker.Action.CompileTest.hi" >>= \ex2 ->
  assertBool ("File .o and hi should exists "++ show cont) (ex && ex2)

