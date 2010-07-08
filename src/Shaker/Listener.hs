module Shaker.Listener(
  listen
  ,initialize
  ,schedule
)
where

import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent
import Shaker.Type
import Shaker.Io

-- | initialize the mvar and launch forks
initialize :: ListenerInput -> IO ListenState
initialize lstInput = do
  mC <- newMVar [] 
  mM <- newMVar [] 
  mJ <- newEmptyMVar 
  idLst <- forkIO $ forever $ listen mC mM mJ
  idSch <- forkIO $ forever $ schedule lstInput mJ
  return $ ListenState mC mM [idLst,idSch]

-- | manage the job box. Fill it with a job every delay
schedule :: ListenerInput -> Job -> IO()
schedule lstInput mJ = do
  putMVar mJ $ fileListenInfo lstInput
  threadDelay $ delay lstInput
  return ()
     
-- | listen to the job box and process the job
listen :: CurrentFiles -> ModifiedFiles -> Job -> IO ()
listen mC mM mJ = do 
  job <- takeMVar mJ
  curFiles <- readMVar mC 
  (newFiles,modFiles) <- listModifiedAndCreatedFiles job curFiles
  updateFileStat mC mM newFiles modFiles 
  return ()

-- | Update the files status
updateFileStat :: CurrentFiles -> ModifiedFiles -> [FileInfo] -> [FileInfo] -> IO ()
updateFileStat _ _ _ [] = return ()
updateFileStat mC mM curFiles curMod = do
  _ <- swapMVar mC curFiles 
  putMVar mM curMod 
  return()  

