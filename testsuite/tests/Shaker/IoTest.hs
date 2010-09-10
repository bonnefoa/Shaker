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

type ExpectedFiles = [FilePath]
type PresentFiles = [FilePath]

abstractHunitTestListFiles :: FileListenInfo -> (PresentFiles -> ExpectedFiles -> Bool) -> IO Bool
abstractHunitTestListFiles fli predicat = do 
  normal_list <- listFiles fli{ignore= []}
  res <- listFiles fli
  return $ predicat normal_list res

type ModifyFileInfo = [FileInfo] -> [FileInfo]
type Predicat = [FileInfo] -> [FileInfo] ->Bool

abstractTestModifiedFiles :: FileListenInfo -> ModifyFileInfo -> Predicat -> IO Bool
abstractTestModifiedFiles fli proc predicat= do
     curList <- getCurrentFpCl fli 
     (_,newList) <- listModifiedAndCreatedFiles [fli] (proc curList)
     return $ predicat curList newList 

testListFiles :: Test
testListFiles = TestCase $ do 
  res <- abstractHunitTestListFiles defaultFileListenInfo (\_ b -> length b > 2 )
  res  @? "should have more than 2 files in src/"

testListFilesWithIgnoreAll :: Test
testListFilesWithIgnoreAll = TestCase $ do 
  res <- abstractHunitTestListFiles defaultFileListenInfo {ignore=[".*"]} (\_ b -> b == [] )
  res  @? "List with ignore all should return an empty list"

testListFilesWithIgnore :: Test
testListFilesWithIgnore = TestCase $ do
  res <-  abstractHunitTestListFiles defaultFileListenInfo {ignore=["\\.$"]} (\a b -> length a  ==length b + 2)
  res @? "ignore of \\.$ should exclude only . and .."

testListFilesWithIncludeAll :: Test
testListFilesWithIncludeAll = TestCase $ do 
  res <- abstractHunitTestListFiles defaultFileListenInfo {include=[".*"]} (\a b->length a == length b)
  res @? "inclue of .* should should list all files"

testListModifiedFiles :: Test
testListModifiedFiles = TestCase $ do
  res <- abstractTestModifiedFiles defaultFileListenInfo (map modifyFileInfoClock) (\a b -> length a == length b)
  res @? "all modified files should be listed "

testListCreatedFiles :: Test
testListCreatedFiles = TestCase $ do
  res <- abstractTestModifiedFiles defaultFileListenInfo (init) (\_ b -> length b==1)
  res @? "a created file should be listed "

testListModifiedAndCreatedFiles :: Test
testListModifiedAndCreatedFiles = TestCase $ do
  res <- abstractTestModifiedFiles defaultFileListenInfo (map modifyFileInfoClock . init) (\a b -> length a == length b)
  res @? "should list modified and created files"

testRecurseListFiles :: Test
testRecurseListFiles = TestCase $ 
  recurseListFiles (FileListenInfo "." ["\\.$"] []) >>= \res ->
  any ("IoTest.hs" `isSuffixOf`) res @? "Should contains IoTest.hs file "++show res
  
testListHsFiles :: Test
testListHsFiles = TestCase $
  recurseListFiles (FileListenInfo "." [] [".*\\.hs$"]) >>= \res ->
  all (".hs" `isSuffixOf`) res @?  "Should only contains hs files " ++ show res
    
testIsFileContainingMain :: Test
testIsFileContainingMain = TestCase $ do
  res <- isFileContainingMain "prog/Shaker.hs" 
  res @? "File Shaker.hs should contain main methods" 

testIsFileNotContainingMain :: Test
testIsFileNotContainingMain = TestCase $ do
  res <- isFileContainingMain "src/Shaker/Config.hs"
  not res @? "File Config.hs should not contain main methods" 

testIsFileConductorNotContainingMain :: Test
testIsFileConductorNotContainingMain = TestCase $ do
  res <- isFileContainingMain "src/Shaker/Conductor.hs"
  not res @?  "File Config.hs should not contain main methods" 

