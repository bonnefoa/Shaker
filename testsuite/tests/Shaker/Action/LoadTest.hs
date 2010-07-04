module Shaker.Action.LoadTest
 where

import Shaker.Action.Load
import Shaker.Parser
import Shaker.Io 
import Test.HUnit 
import Shaker.Type
import Control.Monad.Trans

test_compileFiles = TestCase $
  recurseListFiles (FileListenInfo "." [] [".*\\.hs$"]) >>= \files ->
  runLoadFiles files >>= \res ->
  case res of 
      Left mes -> assertFailure (show mes) 
      _ -> assertBool "" True

