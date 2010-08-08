module Shaker.Action.QuickCheckTest
 where

import Shaker.Reflexivite
import Shaker.Action.QuickCheck
import Test.HUnit
import Control.Monad.Reader
import Shaker.CommonTest

testRunQuickcheck :: Test
testRunQuickcheck = TestCase $ do
  runReaderT runQuickcheck testShakerInput

