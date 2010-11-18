-- | Manage file listener operation for continuous mode.
-- All communication are made via MVars
module Shaker.Listener(
  listen
  ,initializeListener
  ,schedule
  ,updateFileStat
  ,ListenState(..)
)
where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Shaker.Io
import Shaker.Type

initializeListener :: Shaker IO ListenState
initializeListener =do
  lstInput <- asks shakerListenerInput
  lift $ initialize lstInput

-- | initialize the mvar and launch forks
initialize :: ListenerInput -> IO ListenState
initialize lstInput = do
  mC <- newMVar [] 
  mM <- newEmptyMVar
  mJ <- newEmptyMVar 
  idLst <- forkIO $ forever . handleIOException $ listen mC mM mJ 
  idSch <- forkIO $ forever . handleIOException $ schedule lstInput mJ
  return $ ListenState mC mM [idLst,idSch]

-- | manage the job box. Fill it with a job every listenerInputDelay
schedule :: ListenerInput -> Job -> IO()
schedule lstInput mJ = do
  putMVar mJ $ listenerInputFiles lstInput
  threadDelay $ listenerInputDelay lstInput
  return ()
     
-- | listen to the job box and process the job
listen :: CurrentFiles -> MvModifiedFiles -> Job -> IO () 
listen mC mM mJ = do 
  job <- takeMVar mJ
  curFiles <- readMVar mC 
  (newFiles,modFiles) <- listModifiedAndCreatedFiles job curFiles
  updateFileStat mC mM newFiles modFiles  
  return ()

-- | Update the files status
updateFileStat :: CurrentFiles -> MvModifiedFiles -> [FileInfo] -> [FileInfo] -> IO ()
updateFileStat _ _ _ [] = return ()
updateFileStat mC mM curFiles curMod = do
  _ <- swapMVar mC curFiles 
  _ <- tryPutMVar mM curMod 
  return()  

