module Shaker.RunTest
 where

import Shaker.ListenerTest
import Shaker.ParserTest
import Shaker.RegexTest
import Shaker.IoTest
import Test.QuickCheck
import Control.Monad.Trans

runAll :: IO()
runAll = mapM_ liftIO testLists
testLists :: [IO()]
testLists =[ 
  putStrLn "prop_listFiles" >> quickCheck prop_listFiles, 
  putStrLn "prop_listFilesWithIgnoreAll" >> quickCheck prop_listFilesWithIgnoreAll, 
  putStrLn "prop_listFilesWithIgnore" >> quickCheck prop_listFilesWithIgnore, 
  putStrLn "prop_listFilesWithIncludeAll" >> quickCheck prop_listFilesWithIncludeAll, 
  putStrLn "prop_listModifiedFiles" >> quickCheck prop_listModifiedFiles, 
  putStrLn "prop_listCreatedFiles" >> quickCheck prop_listCreatedFiles, 
  putStrLn "prop_listModifiedAndCreatedFiles" >> quickCheck prop_listModifiedAndCreatedFiles, 
  putStrLn "prop_updateFileStat" >> quickCheck prop_updateFileStat, 
  putStrLn "prop_schedule" >> quickCheck prop_schedule, 
  putStrLn "prop_listen" >> quickCheck prop_listen, 
  putStrLn "prop_parseDefaultAction" >> quickCheck prop_parseDefaultAction, 
  putStrLn "prop_filterListAll" >> quickCheck prop_filterListAll, 
  putStrLn "prop_filterListNone" >> quickCheck prop_filterListNone, 
  putStrLn "prop_filterListPartial" >> quickCheck prop_filterListPartial, 
  putStrLn "prop_includeAll" >> quickCheck prop_includeAll, 
  putStrLn "prop_getIncludedAll_all" >> quickCheck prop_getIncludedAll_all, 
  putStrLn "prop_getIncludedAll_none" >> quickCheck prop_getIncludedAll_none
  ]

