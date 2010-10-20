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

import Control.Monad
import Control.Monad.Reader
import Control.Concurrent.MVar
import Control.Concurrent

import Shaker.Type
import Shaker.Io

-- | MVar used to store currentFiles listed
type CurrentFiles = MVar [FileInfo]
-- | MVar used to store modifiedFiles since the last check
type MvModifiedFiles = MVar [FileInfo]
-- | MVar used to pass action to the fileListenInfoDirectory scanner
type Job = MVar [FileListenInfo]

-- | Agregate all information of listener
data ListenState = ListenState {
  currentFiles :: CurrentFiles  -- ^ Files found in the last check
  ,mvModifiedFiles :: MvModifiedFiles -- ^ Differences between last and before last check
  ,threadIds :: [ThreadId] -- ^ List of all forks id initialized
}

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

