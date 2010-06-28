module Shaker.ParseTest
 where

import Shaker.Parse
import Shaker.Io 
import Test.HUnit 
import Shaker.Type
import Control.Monad.Trans

test_compileFiles = TestCase $
  listFiles (FileListenInfo "." [] [".*\\.hs"]) >>= \files ->
  runLoadFiles files >>
  assertBool "ga" True

