module Shaker.IoTest
 where

import Control.Monad.Reader
import Data.List
import qualified Data.Map as M
import Shaker.CommonTest
import Shaker.Io
import Shaker.Type
import System.Time
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
  normal_list <- listFiles fli{fileListenInfoIgnore= []}
  res <- listFiles fli
  return $ predicat normal_list res

type ModifyFileInfo = [FileInfo] -> [FileInfo]
type Predicat = [FileInfo] -> [FileInfo] ->Bool

abstractTestModifiedFiles :: FileListenInfo -> ModifyFileInfo -> Predicat -> IO Bool
abstractTestModifiedFiles fli proc predicat= do
     curList <- getCurrentFpCl fli 
     (_,newList) <- listModifiedAndCreatedFiles [fli] (proc curList)
     return $ predicat curList newList 

testListFiles :: Assertion
testListFiles =  do 
  res <- abstractHunitTestListFiles defaultFileListenInfo (\_ b -> length b > 2 )
  res  @? "should have more than 2 files in src/"

testListFilesWithIgnoreAll :: Assertion
testListFilesWithIgnoreAll =  do 
  res <- abstractHunitTestListFiles defaultFileListenInfo {fileListenInfoIgnore=[".*"]} (\_ b -> b == [] )
  res  @? "List with fileListenInfoIgnore all should return an empty list"

testListFilesWithIgnore :: Assertion
testListFilesWithIgnore =  do
  res <-  abstractHunitTestListFiles defaultFileListenInfo {fileListenInfoIgnore=["\\.$"]} (\a b -> length a  ==length b + 2)
  res @? "fileListenInfoIgnore of \\.$ should exclude only . and .."

testListFilesWithIncludeAll :: Assertion
testListFilesWithIncludeAll =  do 
  res <- abstractHunitTestListFiles defaultFileListenInfo {fileListenInfoInclude=[".*"]} (\a b->length a == length b)
  res @? "inclue of .* should should list all files"

testListModifiedFiles :: Assertion
testListModifiedFiles =  do
  res <- abstractTestModifiedFiles defaultFileListenInfo (map modifyFileInfoClock) (\a b -> length a == length b)
  res @? "all modified files should be listed "

testListCreatedFiles :: Assertion
testListCreatedFiles =  do
  res <- abstractTestModifiedFiles defaultFileListenInfo init (\_ b -> length b==1)
  res @? "a created file should be listed "

testListModifiedAndCreatedFiles :: Assertion
testListModifiedAndCreatedFiles =  do
  res <- abstractTestModifiedFiles defaultFileListenInfo (map modifyFileInfoClock . init) (\a b -> length a == length b)
  res @? "should list modified and created files"

testRecurseListFiles :: Assertion
testRecurseListFiles =  
  recurseListFiles (FileListenInfo "." ["\\.$"] []) >>= \res ->
  any ("IoTest.hs" `isSuffixOf`) res @? "Should contains IoTest.hs file "++show res
  
testListHsFiles :: Assertion
testListHsFiles = 
  recurseListFiles (FileListenInfo "." [] [".*\\.hs$"]) >>= \res ->
  all (".hs" `isSuffixOf`) res @?  "Should only contains hs files " ++ show res
    
testMapImportToModules :: Assertion
testMapImportToModules = do
  map_import_modules <- fmap packageDataMapImportToModules mapImportToModules
  let data_list_import =  map_import_modules M.! "System.Time"
  any (== "Shaker.IoTest"  ) data_list_import @? show data_list_import

testGetCorrespondingBuildFile :: Assertion
testGetCorrespondingBuildFile = do 
  shIn <- testShakerInput
  res <- runReaderT (getCorrespondingBuildFile "src/test.test") shIn
  "dist/shakerTarget/src/test" `isSuffixOf` res @? show res

