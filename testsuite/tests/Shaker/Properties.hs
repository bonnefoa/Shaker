module Shaker.Properties
 where 

import Control.Monad
import Test.QuickCheck 
import Shaker.Type
import System.Time
import Shaker.Reflexivite

instance Arbitrary TimeDiff where
   arbitrary =  TimeDiff `liftM` elements tab
			 `ap` elements tab
			 `ap` elements tab
			 `ap` elements tab
			 `ap` elements tab
			 `ap` elements tab
			 `ap` elements (map fromIntegral tab)
     where tab = [1..10] 

instance Arbitrary ClockTime where
   arbitrary = TOD `liftM` elements [1..1000]
		   `ap` elements [1..1000]

instance Arbitrary FileListenInfo where 
   arbitrary = FileListenInfo `liftM` elements ["src","testsuite"]
			      `ap` (listOf . elements) ["\\.$","ab"]
			      `ap` elements [[],[".*"]]

instance Arbitrary FileInfo where
   arbitrary = arbitrary >>= \cl ->
               elements [".",".."] >>= \ele ->
               return $ FileInfo ele cl
instance Arbitrary ModuleMapping where 
  arbitrary = do 
              name <- createShortName
              listHunitName <- listOf createShortName 
              listPropName <- listOf createShortName 
              return $ ModuleMapping name listHunitName listPropName

createListName :: Gen [String]
createListName = do 
 sizeList <- number
 vectorOf sizeList createShortName
 where number = elements [0..10]

createShortName :: Gen String
createShortName = do
 sizeName <- number
 vectorOf sizeName letters
 where number = elements [0..10]
       letters = elements $ '.' : ['a'..'z'] 

