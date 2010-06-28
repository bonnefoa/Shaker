module Shaker.Run
 where

import Shaker.ListenerTest
import Shaker.ParseTest
import Shaker.RegexTest
import Shaker.IoTest
import Test.QuickCheck

--runAll = mapM quickCheck testLists

testLists =[ 
  prop_listFiles,
  prop_listFilesWithIgnoreAll,
  prop_listFilesWithIgnore,
  prop_listFilesWithIncludeAll,
  prop_listModifiedFiles,
  prop_listCreatedFiles,
  prop_listModifiedAndCreatedFiles,
  prop_filterListAll,
  prop_filterListNone,
  prop_filterListPartial,
  prop_includeAll,
  prop_getIncludedAll_all,
  prop_getIncludedAll_none
  ]

monadTestList = [
  prop_updateFileStat,
  prop_schedule,
  prop_listen
  ]
  
