module Shaker.ListenerTest
where

import System.Time
import Control.Concurrent.MVar
import Shaker.Listener
import Shaker.Type
import Test.QuickCheck 
import Test.QuickCheck.Monadic 

prop_updateFileStat mC mM curF curM = monadicIO test
  where test = run (updateFileStat mC mM curF curM) >>
               run (readMVar mC) >>= \mCurF ->
               assert $  curF == mCurF

instance Arbitrary ClockTime where
   arbitrary = elements [1..1000] >>= \sec ->
               elements [1..1000] >>= \pico ->
               return $ TOD sec pico

instance Arbitrary FileInfo where
   arbitrary = arbitrary >>= \cl ->
               elements [".",".."] >>= \ele ->
               return $ (FileInfo ele cl)

