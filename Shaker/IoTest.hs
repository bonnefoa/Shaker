module Shaker.IoTest
 where 

import Shaker.Io 
import Test.QuickCheck
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

testList = map TestCase [testListFiles,testListFilesWithIgnore, testListFilesWithIgnoreAll ]

tests = TestList testList 

