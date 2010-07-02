module Shaker.Listener
where

import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent
import Shaker.Type
import Shaker.Io


defaultDelay = 2*10^6

listenProjectFiles :: IO(ListenState) 
listenProjectFiles = initialize $ FileListenInfo "." [] [".*\\.hs$"] 


-- | listen to the job box and process the job
listen :: CurrentFiles -> ModifiedFiles -> Job -> IO ()
listen mC mM mJ = takeMVar mJ  >>= \job ->
  readMVar mC >>= \curFiles ->
  readMVar mM >>= \curMod ->
  listModifiedAndCreatedFiles job curFiles >>= \(newFiles,modFiles) ->
  updateFileStat mC mM newFiles modFiles >>
  return ()

-- | Update the files status
updateFileStat :: CurrentFiles -> ModifiedFiles -> [FileInfo] -> [FileInfo] -> IO ()
updateFileStat mC mM curFiles [] = return ()
updateFileStat mC mM curFiles curMod =
  putStrLn ("Modified files ::"++ (show curMod) )>>
  swapMVar mC curFiles >>
  swapMVar mM curMod >>
  return()  

-- | initialize the mvar and launch forks
initialize :: FileListenInfo -> IO (ListenState)
initialize fli =
  newMVar [] >>= \mC ->
  newMVar [] >>= \mM ->
  newEmptyMVar >>= \mJ ->
  (forkIO $ forever $ listen mC mM mJ) >>= \idLst ->
  (forkIO $ forever $ schedule defaultDelay fli mJ) >>= \idSch ->
  return $ ListenState mC mM idLst idSch

-- | manage the job box. Fill it with a job every delay
schedule :: Int -> FileListenInfo -> Job -> IO()
schedule delay fileListenInfo mJ =
  putMVar mJ fileListenInfo >>
  threadDelay delay >>
  return ()
     
