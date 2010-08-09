module Shaker.Action.QuickCheckTest
 where

import Shaker.Action.QuickCheck
import Test.HUnit
import Control.Monad.Reader
import Shaker.CommonTest

testRunQuickCheck :: Test
testRunQuickCheck = TestCase $ 
  runReaderT runQuickCheck testShakerInput

