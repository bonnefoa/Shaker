module Shaker.CompileActionTest
 where

import Shaker.CompileAction
import Test.HUnit
import System.Directory

test_runCompile = TestCase $ 
  runCompile "Shaker.Parse" >> 
  doesFileExist "target/Shaker/Parser.o" >>= \ex -> 
  assertBool ("File should exists ") ex
  
