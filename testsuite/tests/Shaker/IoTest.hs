module Shaker.IoTest
 where

import Shaker.Io
import System.Time
import Data.List
import Test.QuickCheck 
import Test.QuickCheck.Monadic 
import Test.HUnit hiding (assert)

aTimeDiff :: TimeDiff
aTimeDiff = TimeDiff { tdYear = 0, tdMonth = 0, tdDay = 0, tdHour =0, tdMin=0, tdSec = -1, tdPicosec =0 }

modifyFileInfoClock :: FileInfo -> FileInfo
modifyFileInfoClock (FileInfo fp cl) = FileInfo fp (addToClockTime aTimeDiff cl)

abstractTestListFiles :: FileListenInfo -> ([FilePath] -> [FilePath] -> Bool) -> Property
abstractTestListFiles fli predicat = monadicIO action
  where action = do
               lstFile <- run $ listFiles fli{ignore= []}
               r <- run $listFiles fli
               assert $ predicat lstFile r 

prop_listFiles :: FileListenInfo -> Property
prop_listFiles fli = abstractTestListFiles fli{ignore=[]} (\_ b -> length b>2)

prop_listFilesWithIgnoreAll :: FileListenInfo -> Property
prop_listFilesWithIgnoreAll fli = abstractTestListFiles fli{ignore= [".*"], include=[]} (\_ b -> b==[])

prop_listFilesWithIgnore :: FileListenInfo -> Property
prop_listFilesWithIgnore fli = abstractTestListFiles fli{ignore= ["\\.$"], include=[]} (\a b-> length a == length b + 2)

prop_listFilesWithIncludeAll :: FileListenInfo -> Property
prop_listFilesWithIncludeAll fli = abstractTestListFiles fli{include=[".*"]} (\a b->length a >= length b)

testModifiedFiles :: FileListenInfo -> ([FileInfo] -> [FileInfo]) -> ([FileInfo] -> [FileInfo] ->Bool) -> Property
testModifiedFiles fli proc predicat= monadicIO action
  where action = do 
               curList <- run $ getCurrentFpCl fli 
               (_,newList) <- run $ listModifiedAndCreatedFiles [fli] (proc curList)
               assert $ predicat curList newList 

prop_listModifiedFiles :: FileListenInfo -> Property
prop_listModifiedFiles fli = 
  testModifiedFiles fli (map modifyFileInfoClock) (\a b -> length a == length b)

prop_listCreatedFiles :: FileListenInfo -> Property
prop_listCreatedFiles fli = 
  testModifiedFiles fli init (\_ b -> length b==1)

prop_listModifiedAndCreatedFiles :: FileListenInfo -> Property
prop_listModifiedAndCreatedFiles fli = 
  testModifiedFiles fli (map modifyFileInfoClock . init) (\a b -> length a == length b)

testRecurseListFiles :: Test
testRecurseListFiles = TestCase $ 
  recurseListFiles (FileListenInfo "." ["\\.$"] []) >>= \res ->
  assertBool ("Should contains IoTest.hs file "++show res) $
    any ("IoTest.hs" `isSuffixOf`) res
  
testListFiles :: Test
testListFiles = TestCase $ 
  listFiles (FileListenInfo "." [] []) >>= \res ->
  assertBool ("Should contains src dir"++show res) $
    any ("src" `isSuffixOf`) res
  
testListHsFiles :: Test
testListHsFiles = TestCase $
  recurseListFiles (FileListenInfo "." [] [".*\\.hs$"]) >>= \res ->
  assertBool ("Should only contains hs files " ++ show res) $
    all (".hs" `isSuffixOf`) res

testIsFileContainingMain :: Test
testIsFileContainingMain = TestCase $ do
  res <- isFileContainingMain "src/Main.hs" 
  assertBool "File Main.hs should contain main methods" res

testIsFileNotContainingMain :: Test
testIsFileNotContainingMain = TestCase $ do
  res <- isFileContainingMain "src/Shaker/Config.hs"
  assertBool "File Config.hs should not contain main methods" $ not res



