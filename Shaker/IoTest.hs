module Shaker.IoTest
 where 

import Shaker.Io 
import Test.QuickCheck
import Test.HUnit
import Data.List

simpleTest = assertEqual "for (foo 3)," (1,2) (1,2)

testListFiles = listFiles "." [] >>= \res -> 
  assertBool "list files should be greater than 2 "
    $ length res > 2

testListFilesWithIgnore = listFiles "." ignoreList >>= \res -> 
  assertBool "list files should ignore files passed"
    $ res `intersect` ignoreList == []
  where ignoreList =[".", ".."] 




testList = map TestCase [testListFiles,testListFilesWithIgnore]

tests = TestList testList 

