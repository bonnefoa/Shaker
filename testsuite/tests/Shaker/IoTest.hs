module Shaker.IoTest
 where

import Shaker.Type
import Shaker.Io
import System.Directory
import System.Time
import Test.HUnit
import Data.List

aTimeDiff :: TimeDiff
aTimeDiff = TimeDiff { tdYear = 0, tdMonth = 0, tdDay = 0, tdHour =0, tdMin=0, tdSec = -1, tdPicosec =0 }

modifyFileInfoClock :: FileInfo -> FileInfo
modifyFileInfoClock (FileInfo fp cl) = FileInfo fp (addToClockTime aTimeDiff cl)
aSimpleFileListen = FileListenInfo "." []
testListFiles = listFiles aSimpleFileListen >>= \res ->
    assertBool "list files should be greater than 2 "
        $ length res > 2

testListFilesWithIgnore = listFiles (FileListenInfo "." ignoreList) >>= \res ->
    listFiles aSimpleFileListen >>= \resWoIgnore ->
    assertEqual "list files should ignore files given"
         (length res) (length resWoIgnore - 2)
          where ignoreList =["\\.$", "\\.\\.$"]

testListFilesWithIgnoreAll = listFiles (FileListenInfo "." [".*"]) >>= \res ->
    assertEqual "ignore all files should result in empty list"
      0 (length res)

testListModifiedFiles = 
    getCurrentFpCl aSimpleFileListen >>= \curList ->
    listModifiedFiles (map modifyFileInfoClock curList) >>= \newList ->
    assertEqual "should have one file modified"
      (length curList) (length newList) 

testListCreatedFiles = 
    getCurrentFpCl aSimpleFileListen >>= \curList ->
    listCreatedFiles aSimpleFileListen (init curList) >>= \newList ->
    assertEqual "should have one file created"
      1 (length newList) 

testListModifiedAndCreatedFiles = 
    listModifiedAndCreatedFiles aSimpleFileListen [] >>= \currentList ->
    listModifiedAndCreatedFiles aSimpleFileListen (map modifyFileInfoClock $ init currentList) >>= \modifiedAndCreatedList ->
      assertEqual "should be some modified files and one created file" 
        (length modifiedAndCreatedList) (length currentList)

testList = map TestCase [testListFiles,testListFilesWithIgnore, testListFilesWithIgnoreAll, testListModifiedFiles , testListCreatedFiles, testListModifiedAndCreatedFiles ]

tests = TestList testList



