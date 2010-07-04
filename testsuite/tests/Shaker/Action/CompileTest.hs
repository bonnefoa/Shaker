module Shaker.Action.CompileTest
 where

import Shaker.Action.Compile
import Test.HUnit
import System.Directory

test_runCompile = TestCase $ 
  runCompile ["Shaker.Parser"] >> 
  doesFileExist "target/Shaker/Parser.o" >>= \ex -> 
  assertBool ("File .o should exists ") ex
  
test_runCompileProject = TestCase $ 
  runCompileProject >>
  getDirectoryContents "target/Shaker" >>= \cont ->
  doesFileExist "target/Shaker.Action.CompileTest.o" >>= \ex ->
  doesFileExist "target/Shaker.Action.CompileTest.hi" >>= \ex2 ->
  assertBool ("File .o and hi should exists "++ show cont) (ex && ex2)

