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

testListFiles = listFiles "." [] >>= \res ->
    assertBool "list files should be greater than 2 "
        $ length res > 2

testListFilesWithIgnore = listFiles "." ignoreList >>= \res ->
    listFiles "." [] >>= \resWoIgnore ->
    assertEqual "list files should ignore files given"
         (length res) (length resWoIgnore - 2)
          where ignoreList =["\\.$", "\\.\\.$"]

testListFilesWithIgnoreAll = listFiles "." [".*"] >>= \res ->
    assertEqual "ignore all files should result in empty list"
      0 (length res)

testListModifiedFiles = 
    getCurrentFpCl "." [] >>= \curList ->
    listModifiedFiles (map modifyFileInfoClock curList) >>= \newList ->
    assertEqual "should have one file modified"
      (length curList) (length newList) 

testListCreatedFiles = 
    getCurrentFpCl "." [] >>= \curList ->
    listCreatedFiles "." [] (init curList) >>= \newList ->
    assertEqual "should have one file created"
      1 (length newList) 

testList = map TestCase [testListFiles,testListFilesWithIgnore, testListFilesWithIgnoreAll, testListModifiedFiles , testListCreatedFiles ]

tests = TestList testList



