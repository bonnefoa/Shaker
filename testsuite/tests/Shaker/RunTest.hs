module Shaker.RunTest
 where

import Shaker.Cabal.CabalInfoTest
import Shaker.Action.CompileTest
import Shaker.CliTest
import Shaker.ListenerTest
import Shaker.ParserTest
import Shaker.RegexTest
import Shaker.IoTest
import Test.QuickCheck
import Test.HUnit
import Control.Monad.Trans

runAll :: IO()
runAll = mapM_ liftIO propLists

propLists :: [IO()]
propLists =[ 
    putStrLn "prop_completeWord ">> quickCheck prop_completeWord,
    putStrLn "prop_partialWords ">> quickCheck prop_partialWords,
    putStrLn "prop_completeMultipleWords ">> quickCheck prop_completeMultipleWords,
    putStrLn "prop_partialMultipleWords ">> quickCheck prop_partialMultipleWords,
    putStrLn "prop_listFiles ">> quickCheck prop_listFiles,
    putStrLn "prop_listFilesWithIgnoreAll ">> quickCheck prop_listFilesWithIgnoreAll,
    putStrLn "prop_listFilesWithIgnore ">> quickCheck prop_listFilesWithIgnore,
    putStrLn "prop_listFilesWithIncludeAll ">> quickCheck prop_listFilesWithIncludeAll,
    putStrLn "prop_listModifiedFiles ">> quickCheck prop_listModifiedFiles,
    putStrLn "prop_listCreatedFiles ">> quickCheck prop_listCreatedFiles,
    putStrLn "prop_listModifiedAndCreatedFiles ">> quickCheck prop_listModifiedAndCreatedFiles,
    putStrLn "prop_updateFileStat ">> quickCheck prop_updateFileStat,
    putStrLn "prop_schedule ">> quickCheck prop_schedule,
    putStrLn "prop_listen ">> quickCheck prop_listen,
    putStrLn "prop_parseDefaultAction ">> quickCheck prop_parseDefaultAction,
    putStrLn "prop_parseCommand ">> quickCheck prop_parseCommand,
    putStrLn "prop_filterListAll ">> quickCheck prop_filterListAll,
    putStrLn "prop_filterListNone ">> quickCheck prop_filterListNone,
    putStrLn "prop_filterListPartial ">> quickCheck prop_filterListPartial,
    putStrLn "prop_includeAll ">> quickCheck prop_includeAll,
    putStrLn "prop_getIncludedAll_all ">> quickCheck prop_getIncludedAll_all,
    putStrLn "prop_getIncludedAll_none ">> quickCheck prop_getIncludedAll_none
  ]

testLists :: [IO Counts]
testLists = [
   putStrLn "testRunCompileProject" >> runTestTT testRunCompileProject,
   putStrLn "testParseCabalConfig " >> runTestTT testParseCabalConfig ,
   putStrLn "testInvalidMainShouldBeExcluded " >> runTestTT testInvalidMainShouldBeExcluded ,
   putStrLn "testCompileWithLocalSource " >> runTestTT testCompileWithLocalSource ,
   putStrLn "testProjectCabalContentWithLocalSource " >> runTestTT testProjectCabalContentWithLocalSource ,
   putStrLn "testRecurseListFiles " >> runTestTT testRecurseListFiles ,
   putStrLn "testListFiles " >> runTestTT testListFiles ,
   putStrLn "testListHsFiles " >> runTestTT testListHsFiles ,
   putStrLn "testIsFileContainingMain " >> runTestTT testIsFileContainingMain ,
   putStrLn "testIsFileNotContainingMain " >> runTestTT testIsFileNotContainingMain 
  ]

