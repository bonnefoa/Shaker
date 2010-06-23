module Shaker.ListenerTest
where

import Control.Concurrent.MVar
import Control.Monad
import Shaker.Listener
import Shaker.Properties
import Test.QuickCheck 
import Test.QuickCheck.Monadic 

prop_updateFileStat curF curM = not (null curM) ==>
  monadicIO (test curF curM)
  where test curF curM = 
          run (newMVar []) >>= \mC ->    
          run (newMVar []) >>= \mM ->    
          run (updateFileStat mC mM curF curM) >>
          run (readMVar mC) >>= \mCurF ->
          assert $  curF == mCurF

