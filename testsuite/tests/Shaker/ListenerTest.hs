module Shaker.ListenerTest
where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Shaker.Listener
import Shaker.Properties
import Test.QuickCheck 
import Test.QuickCheck.Monadic 
import Shaker.Type

prop_updateFileStat curF curM = not (null curM) ==>
  monadicIO (test curF curM)
  where test curF curM = 
          run (newMVar []) >>= \mC ->    
          run (newMVar []) >>= \mM ->    
          run (updateFileStat mC mM curF curM) >>
          run (readMVar mC) >>= \mCurF ->
          assert $  curF == mCurF

prop_schedule :: FileListenInfo -> Property
prop_schedule fli = monadicIO $ test fli
  where test fli = run newEmptyMVar  >>= \mJ -> 
  		   run (schedule 0 fli mJ) >> 
  		   run (tryTakeMVar mJ) >>= \res ->
		   assert $ res == (Just fli)

prop_listen fli = monadicIO $ test fli
 where test fli = 
	run (newMVar []) >>= \mC ->
	run (newMVar []) >>= \mM ->
	run (newMVar fli) >>= \mJ ->
	run (listen mC mM mJ) >>
	run (tryTakeMVar mC) >>= \res ->
	assert $ True
