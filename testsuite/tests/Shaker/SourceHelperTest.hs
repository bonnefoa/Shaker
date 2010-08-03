module Shaker.SourceHelperTest
 where

import Control.Monad.Reader
import System.Directory
import Test.HUnit
import Shaker.Type

testConstuctCompileFileList :: Test
testConstuctCompileFileList = TestCase $ do
  let cpIn = CompileInput {cfSourceDirs = ["src"]}
  False @? ""

