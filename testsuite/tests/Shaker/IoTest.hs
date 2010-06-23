module Shaker.IoTest
 where

import Shaker.Type
import Shaker.Io
import System.Directory
import System.Time
import Control.Monad
import Data.List
import Test.QuickCheck 
import Test.QuickCheck.Monadic 
import Shaker.Properties

aTimeDiff :: TimeDiff
aTimeDiff = TimeDiff { tdYear = 0, tdMonth = 0, tdDay = 0, tdHour =0, tdMin=0, tdSec = -1, tdPicosec =0 }

modifyFileInfoClock :: FileInfo -> FileInfo
modifyFileInfoClock (FileInfo fp cl) = FileInfo fp (addToClockTime aTimeDiff cl)
aSimpleFileListen = FileListenInfo "." []


prop_listFiles :: FileListenInfo -> Property
prop_listFiles fli = monadicIO test
	where test = run (listFiles fli{ignore = []}) >>= \r ->
	      	     assert $ length r>2

prop_listFilesWithIgnoreAll :: FileListenInfo -> Property
prop_listFilesWithIgnoreAll fli = monadicIO test
	where test = run (listFiles fli{ignore= [".*"]}) >>= \r->
	             assert $ r == []

prop_listFilesWithIgnore :: FileListenInfo -> Property
prop_listFilesWithIgnore fli = monadicIO test
	where test = run (listFiles fli) >>= \lstFile ->
		     run (listFiles fli{ignore= ["\\.$","\\.\\.$"]}) >>= \r->
	             assert $ length r == length lstFile - 2

{-
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
    listModifiedAndCreatedFiles aSimpleFileListen (map modifyFileInfoClock curList) >>= \newList ->
    assertEqual "should have one file modified"
      (length curList) (length newList) 

testListCreatedFiles = 
    getCurrentFpCl aSimpleFileListen >>= \curList ->
    listModifiedAndCreatedFiles aSimpleFileListen (init curList) >>= \newList ->
    assertEqual "should have one file created"
      1 (length newList) 

testListModifiedAndCreatedFiles = 
    listModifiedAndCreatedFiles aSimpleFileListen [] >>= \currentList ->
    listModifiedAndCreatedFiles aSimpleFileListen (map modifyFileInfoClock $ init currentList) >>= \modifiedAndCreatedList ->
      assertEqual "should be some modified files and one created file" 
        (length modifiedAndCreatedList) (length currentList)

-}


