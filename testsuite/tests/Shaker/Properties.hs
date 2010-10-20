module Shaker.Properties
 where 

import Control.Monad
import Test.QuickCheck 
import Shaker.Type
import System.Time
import Shaker.Reflexivite

instance Arbitrary TimeDiff where
   arbitrary =  TimeDiff `liftM` genSmallNumber
			 `ap` genSmallNumber 
			 `ap` genSmallNumber 
			 `ap` genSmallNumber 
			 `ap` genSmallNumber 
			 `ap` genSmallNumber 
			 `ap` elements [1..10] 

instance Arbitrary ClockTime where
   arbitrary = TOD `liftM` elements [1..1000]
		   `ap` elements [1..1000]

instance Arbitrary FileListenInfo where 
   arbitrary = do
     gen_fileListenInfoDir <- elements ["src","testsuite"]
     sizeIgnore <- genSmallNumber
     gen_fileListenInfoIgnore <- vectorOf sizeIgnore $ elements ["\\.$","ab"]
     gen_fileListenInfoInclude <- elements [[],[".*"]]
     return $ FileListenInfo gen_fileListenInfoDir gen_fileListenInfoIgnore gen_fileListenInfoInclude

instance Arbitrary FileInfo where
   arbitrary = arbitrary >>= \cl ->
               elements [".",".."] >>= \ele ->
               return $ FileInfo ele cl

instance Arbitrary ModuleMapping where 
  arbitrary = do 
              name <- createShortName
              listHunitName <- listOf createShortName 
              listPropName <- listOf createShortName 
              listTestCases <- listOf createShortName 
              return $ ModuleMapping name listHunitName listTestCases listPropName

genSmallNumber :: Gen Int
genSmallNumber = elements [0..10]

createListName :: Gen [String]
createListName = do 
 sizeList <- genSmallNumber
 vectorOf sizeList createShortName

createShortName :: Gen String
createShortName = do
 sizeName <- genSmallNumber
 vectorOf sizeName letters
 where letters = elements $ '.' : ['a'..'z'] 

