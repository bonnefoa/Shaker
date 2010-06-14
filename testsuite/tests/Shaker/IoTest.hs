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
        (length res) 0 

testListModifiedFiles = 
    getCurrentFpCl "." [] >>= \curList ->
    listModifiedFiles (map modifyFileInfoClock curList) >>= \newList ->
    assertBool "should have one file modified"
      (length newList) == 1

testListCreatedFiles = 
    getCurrentFpCl "." [] >>= \curList ->
    listCreatedFiles "." [] (init curList) >>= \newList ->
    assertEqual "should have one file created"
      (length newList) 1 

testList = map TestCase [testListFiles,testListFilesWithIgnore, testListFilesWithIgnoreAll, testListModifiedFiles , testListCreatedFiles ]

tests = TestList testList

