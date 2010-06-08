module Shaker.IoTest
 where

import Shaker.Io
import System.Directory
import Test.HUnit
import Data.List

testListFiles = listFiles "." [] >>= \res ->
    assertBool "list files should be greater than 2 "
        $ length res > 2

testListFilesWithIgnore = listFiles "." ignoreList >>= \res ->
    assertBool "list files should ignore files given"
        $ res `intersect` ignoreList == []
          where ignoreList =["\\.", "\\.\\."]

testListFilesWithIgnoreAll = listFiles "." [".*"] >>= \res ->
    assertBool "ignore all files should result in empty list"
        $ length res == 0 

testListModifiedFiles = getTemporaryDirectory >>= \tmpDir ->
      writeFile "test.txt" "testContent"  >>
            getCurrentFpCl tmpDir ["^\\."] >>= \curList ->
                  writeFile "test.txt" "testContentBis"  >>
                        listModifiedFiles curList >>= \newList ->
                              assertBool "should have at least one file modified"
                                    $ length newList > 0
                                       

testListCreatedFiles = getTemporaryDirectory >>= \tmpDir ->
      getCurrentFpCl tmpDir ["^\\."] >>= \curList ->
            writeFile "test.txt" "testContent"  >>
                  listModifiedFiles curList >>= \newList ->
                        assertBool "should have at least one file create"
                              $ length newList > 0

testList = map TestCase [testListFiles,testListFilesWithIgnore, testListFilesWithIgnoreAll, testListModifiedFiles , testListCreatedFiles ]

tests = TestList testList

