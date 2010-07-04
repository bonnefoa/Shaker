module Shaker.CompileActionTest
 where

import Shaker.CompileAction
import Test.HUnit
import System.Directory

test_runCompile = TestCase $ 
  runCompile ["Shaker.Parser"] >> 
  doesFileExist "target/Shaker/Parser.o" >>= \ex -> 
  assertBool ("File .o should exists ") ex
  
test_runCompileProject = TestCase $ 
  runCompileProject >>
  getDirectoryContents "target/Shaker" >>= \cont ->
  doesFileExist "target/Shaker/CompileActionTest.o" >>= \ex ->
  doesFileExist "target/Shaker/CompileActionTest.hi" >>= \ex2 ->
  assertBool ("File .o and hi should exists "++ show cont) (ex && ex2)

