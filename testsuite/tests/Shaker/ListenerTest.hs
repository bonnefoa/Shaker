module Shaker.ListenerTest
where

import Control.Concurrent
import Shaker.Listener
import Test.HUnit 
import Shaker.Type
import Shaker.Io
import System.Time

testUpdateFileStat :: Assertion
testUpdateFileStat = do
  let clockTime = TOD 100 100
  let curF = [FileInfo "." clockTime]
  let curM = [FileInfo ".." clockTime]
  mC <- newMVar []
  mM <- newEmptyMVar 
  (updateFileStat mC mM curF curM) 
  mCurF <- readMVar mC
  curF == mCurF @? "current file should be equal to the file in mvar"

testSchedule :: Assertion
testSchedule = do
  let fli = FileListenInfo "." [] []
  mJ <- newEmptyMVar 
  schedule (ListenerInput [fli] 0) mJ
  res <- (tryTakeMVar mJ)
  res == Just [fli] @? "scheduled fileListenInfo should be put in the job mvar"

testListen :: Assertion
testListen = do 
  let fli = FileListenInfo "." [] []
  expected <- getCurrentFpCl fli
  mC <- newMVar []
  mM <- newEmptyMVar 
  mJ <- newMVar [fli]
  listen mC mM mJ
  Just res <- tryTakeMVar mC
  expected == res @? "listen should processe the FileListenInfo in the job box"

