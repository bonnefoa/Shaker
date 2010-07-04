module Shaker.Listener
where

import Control.Monad
import Control.Monad.Reader
import Control.Concurrent.MVar
import Control.Concurrent
import Shaker.Type
import Shaker.Io


listenProjectFiles :: IO(ListenState) 
listenProjectFiles = do
  initialize $ ListenerInput {
    fileListenInfo= FileListenInfo "." [] [".*\\.hs$"],
    delay = 2*10^6
   }


-- | listen to the job box and process the job
listen :: CurrentFiles -> ModifiedFiles -> Job -> IO ()
listen mC mM mJ = takeMVar mJ  >>= \job ->
  readMVar mC >>= \curFiles ->
--  readMVar mM >>= \curMod ->
  listModifiedAndCreatedFiles job curFiles >>= \(newFiles,modFiles) ->
  updateFileStat mC mM newFiles modFiles >>
  return ()

-- | Update the files status
updateFileStat :: CurrentFiles -> ModifiedFiles -> [FileInfo] -> [FileInfo] -> IO ()
updateFileStat mC mM curFiles [] = return ()
updateFileStat mC mM curFiles curMod =
  putStrLn ("Modified files ::"++ (show curMod) )>>
  swapMVar mC curFiles >>
  putMVar mM curMod >>
  return()  

-- | initialize the mvar and launch forks
initialize :: ListenerInput -> IO (ListenState)
initialize lstInput = do
  mC <- newMVar [] 
  mM <- newMVar [] 
  mJ <- newEmptyMVar 
  idLst <- forkIO $ forever $ listen mC mM mJ
  idSch <- forkIO $ forever $ schedule lstInput mJ
  return $ ListenState mC mM [idLst,idSch]

-- | manage the job box. Fill it with a job every delay
schedule :: ListenerInput -> Job -> IO()
schedule (ListenerInput fileListenInfo delay) mJ =
  putMVar mJ fileListenInfo >>
  threadDelay delay >>
  return ()
     
