module Shaker.RunTest
 where

import Shaker.ListenerTest
import Shaker.RegexTest
import Shaker.IoTest
import Test.QuickCheck
import Control.Monad.Trans

runAll :: IO()
runAll = mapM_ liftIO testLists
testLists :: [IO()]
testLists =[ 
  quickCheck prop_listFiles, 
  quickCheck prop_listFilesWithIgnoreAll
  ]
