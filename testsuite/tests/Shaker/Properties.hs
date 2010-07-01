module Shaker.Properties
 where 

import Control.Monad
import Control.Monad.Trans
import Test.QuickCheck 
import System.Time
import Shaker.Type
import Shaker.Io

instance Arbitrary TimeDiff where
   arbitrary =  TimeDiff `liftM` elements tab
			 `ap` elements tab
			 `ap` elements tab
			 `ap` elements tab
			 `ap` elements tab
			 `ap` elements tab
			 `ap` (elements $ map fromIntegral tab)
     where tab = [1..10] 

instance Arbitrary ClockTime where
   arbitrary = TOD `liftM` elements [1..1000]
		   `ap` elements [1..1000]

instance Arbitrary FileListenInfo where 
   arbitrary = FileListenInfo `liftM` elements [".",".."]
			      `ap` listOf (elements ["\\.$","ab"])
			      `ap` elements [[],[".*"]]

instance Arbitrary FileInfo where
   arbitrary = arbitrary >>= \cl ->
               elements [".",".."] >>= \ele ->
               return $ (FileInfo ele cl)

instance Arbitrary Action where
  arbitrary = elements [Load,Compile,QuickCheck,Quit,Help]
