module Shaker.CompileActionTest
 where

import Shaker.CompileAction
import Test.HUnit
import System.Directory

test_runCompile = TestCase $ 
  runCompile ["Shaker.Parser"] >> 
  doesFileExist "target/Shaker/Parser.o" >>= \ex -> 
  assertBool ("File should exists ") ex
  
test_runCompileProject = TestCase $ 
  runCompileProject >>
  getDirectoryContents "target/Shaker" >>= \cont ->
  doesFileExist "target/Shaker/CompileActionTest.o" >>= \ex ->
  assertBool ("File should exists "++ show cont) ex

