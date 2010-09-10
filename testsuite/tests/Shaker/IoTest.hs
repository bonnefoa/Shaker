module Shaker.IoTest
 where

import Shaker.Io
import Shaker.Type
import System.Time
import Data.List
import Test.HUnit hiding (assert)

aTimeDiff :: TimeDiff
aTimeDiff = TimeDiff { tdYear = 0, tdMonth = 0, tdDay = 0, tdHour =0, tdMin=0, tdSec = -1, tdPicosec =0 }

modifyFileInfoClock :: FileInfo -> FileInfo
modifyFileInfoClock (FileInfo fp cl) = FileInfo fp (addToClockTime aTimeDiff cl)

defaultFileListenInfo :: FileListenInfo
defaultFileListenInfo = FileListenInfo "src" [] [] 

abstractHunitTestListFiles :: FileListenInfo -> ([FilePath] -> [FilePath] -> Bool) -> IO Bool
abstractHunitTestListFiles fli predicat = do 
  normal_list <- listFiles fli{ignore= []}
  res <- listFiles fli
  return $ predicat normal_list res

abstractTestModifiedFiles :: FileListenInfo -> ([FileInfo] -> [FileInfo]) -> ([FileInfo] -> [FileInfo] ->Bool) -> IO Bool
abstractTestModifiedFiles fli proc predicat= do
     curList <- getCurrentFpCl fli 
     (_,newList) <- listModifiedAndCreatedFiles [fli] (proc curList)
     return $ predicat curList newList 

test_listFiles :: Test
test_listFiles = TestCase $ do 
  res <- abstractHunitTestListFiles defaultFileListenInfo (\_ b -> length b > 2 )
  res  @? "should have more than 2 files in src/"

test_listFilesWithIgnoreAll :: Test
test_listFilesWithIgnoreAll = TestCase $ do 
  res <- abstractHunitTestListFiles defaultFileListenInfo {ignore=[".*"]} (\_ b -> b == [] )
  res  @? "List with ignore all should return an empty list"

test_listFilesWithIgnore :: Test
test_listFilesWithIgnore = TestCase $ do
  res <-  abstractHunitTestListFiles defaultFileListenInfo {ignore=["\\.$"]} (\a b -> length a  ==length b + 2)
  res @? "ignore of \\.$ should exclude only . and .."

test_listFilesWithIncludeAll :: Test
test_listFilesWithIncludeAll = TestCase $ do 
  res <- abstractHunitTestListFiles defaultFileListenInfo {include=[".*"]} (\a b->length a == length b)
  res @? "inclue of .* should should list all files"

test_listModifiedFiles :: Test
test_listModifiedFiles = TestCase $ do
  res <- abstractTestModifiedFiles defaultFileListenInfo (map modifyFileInfoClock) (\a b -> length a == length b)
  res @? "all modified files should be listed "

test_listCreatedFiles :: Test
test_listCreatedFiles = TestCase $ do
  res <- abstractTestModifiedFiles defaultFileListenInfo (init) (\_ b -> length b==1)
  res @? "a created file should be listed "

test_listModifiedAndCreatedFiles :: Test
test_listModifiedAndCreatedFiles = TestCase $ do
  res <- abstractTestModifiedFiles defaultFileListenInfo (map modifyFileInfoClock . init) (\a b -> length a == length b)
  res @? "should list modified and created files"

test_recurseListFiles :: Test
test_recurseListFiles = TestCase $ 
  recurseListFiles (FileListenInfo "." ["\\.$"] []) >>= \res ->
  any ("IoTest.hs" `isSuffixOf`) res @? "Should contains IoTest.hs file "++show res
  
test_listHsFiles :: Test
test_listHsFiles = TestCase $
  recurseListFiles (FileListenInfo "." [] [".*\\.hs$"]) >>= \res ->
  all (".hs" `isSuffixOf`) res @?  "Should only contains hs files " ++ show res
    
test_isFileContainingMain :: Test
test_isFileContainingMain = TestCase $ do
  res <- isFileContainingMain "prog/Shaker.hs" 
  res @? "File Shaker.hs should contain main methods" 

test_isFileNotContainingMain :: Test
test_isFileNotContainingMain = TestCase $ do
  res <- isFileContainingMain "src/Shaker/Config.hs"
  not res @? "File Config.hs should not contain main methods" 

test_isFileConductorNotContainingMain :: Test
test_isFileConductorNotContainingMain = TestCase $ do
  res <- isFileContainingMain "src/Shaker/Conductor.hs"
  not res @?  "File Config.hs should not contain main methods" 

