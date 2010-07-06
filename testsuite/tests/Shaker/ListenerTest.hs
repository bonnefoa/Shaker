module Shaker.ListenerTest
where

import Control.Concurrent
import Shaker.Listener
import Shaker.Properties()
import Test.QuickCheck 
import Test.QuickCheck.Monadic 
import Shaker.Type
import Shaker.Io
    
prop_updateFileStat :: [FileInfo] ->[FileInfo] -> Property
prop_updateFileStat curF curM = not (null curM) ==>
  monadicIO $ do
          mC <- run $ newMVar []
          mM <- run newEmptyMVar 
          run (updateFileStat mC mM curF curM) 
          mCurF <- run $ readMVar mC
          assert $  curF == mCurF

prop_schedule :: FileListenInfo -> Property
prop_schedule fli = monadicIO $ do 
                   mJ <- run newEmptyMVar 
  		   run $ schedule (ListenerInput [fli] 0) mJ
  		   res <- run (tryTakeMVar mJ)
		   assert $ res == Just [fli]

prop_listen :: FileListenInfo -> Property
prop_listen fli = monadicIO $ do
        expected <- run $ getCurrentFpCl fli
	mC <- run $ newMVar []
	mM <- run newEmptyMVar 
	mJ <- run $ newMVar [fli]
	run $ listen mC mM mJ
	Just res <- run $ tryTakeMVar mC
	assert $ expected == res


