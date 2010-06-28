module Shaker.RunTest
 where

import Shaker.ListenerTest
import Shaker.ParseTest
import Shaker.RegexTest
import Shaker.IoTest
import Test.QuickCheck
import Control.Monad
import Control.Monad.Trans

runAll :: IO()
runAll = mapM_ liftIO testLists

testLists =[ 
  quickCheck prop_listFiles,
  quickCheck prop_listFilesWithIgnoreAll,
  quickCheck prop_listFilesWithIgnore,
  quickCheck prop_listFilesWithIncludeAll,
  quickCheck prop_listModifiedFiles,
  quickCheck prop_listCreatedFiles,
  quickCheck prop_listModifiedAndCreatedFiles,
  quickCheck prop_filterListAll,
  quickCheck prop_filterListNone,
  quickCheck prop_filterListPartial,
  quickCheck prop_includeAll,
  quickCheck prop_getIncludedAll_all,
  quickCheck prop_getIncludedAll_none,
  quickCheck prop_updateFileStat,
  quickCheck prop_schedule,
  quickCheck prop_listen
  ]
  
