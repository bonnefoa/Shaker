module Shaker.IoTest
 where

import Shaker.Type
import Shaker.Io
import System.Directory
import Test.HUnit
import Data.List

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

testListModifiedFiles = getTemporaryDirectory >>= \tmpDir ->
  let tmpFile = tmpDir ++"/test.txt" in
    writeFile tmpFile "testContent"  >>
    getCurrentFpCl tmpDir ["\\.$"] >>= \curList ->
    writeFile tmpFile "testContent changed"  >>
    listModifiedFiles curList >>= \newList ->
    assertBool "should have at least one file modified"
      $ length newList > 0

testListCreatedFiles = getTemporaryDirectory >>= \tmpDir ->
  let tmpFile = tmpDir ++"/test.txt" in
    getCurrentFpCl tmpDir ignoreList >>= \curList ->
    writeFile tmpFile "testContent"  >>
    listCreatedFiles tmpDir ignoreList  curList >>= \newList ->
    assertBool "should have at least one file create"
      $ length newList > 0
  where ignoreList = ["\\.$"]

testList = map TestCase [testListFiles,testListFilesWithIgnore, testListFilesWithIgnoreAll, testListModifiedFiles , testListCreatedFiles ]

tests = TestList testList

