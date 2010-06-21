module Shaker.ListenerTest
where

import System.Time
import Control.Concurrent.MVar
import Shaker.Listener
import Shaker.Type
import Test.QuickCheck 
--import Test.QuickCheck (arbitrary, Property, quickCheck, (==>))
import Test.QuickCheck.Monadic 

prop_updateFileStat mC mM curF curM = monadicIO test
  where test = run (updateFileStat mC mM curF curM) >>
               run (readMVar mC) >>= \mCurF ->
               assert $  curF == mCurF

{-
instance Arbitrary FileInfo where
  arbitrary = elements [".",".."] >>= \dir ->
              arbitrary 
              FileInfo dir (TOD arbitrary arbitrary)  
-}

