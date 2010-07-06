module Shaker.Action.LoadTest
 where

import Shaker.Action.Load
import Shaker.Io 
import Test.HUnit 
import Shaker.Type

testCompileFiles :: Test
testCompileFiles = TestCase $
  recurseListFiles (FileListenInfo "." [] [".*\\.hs$"]) >>= \files ->
  runLoadFiles files >>= \res ->
  case res of 
      Left mes -> assertFailure (show mes) 
      _ -> assertBool "" True

