module Shaker.Listener
where

import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent
import Shaker.Type
import Shaker.Io

type CurrentFiles = MVar [FileInfo]
type ModifiedFiles = MVar [FileInfo]
type Job = MVar FileListenInfo

delay = 5*10^6

listen :: CurrentFiles -> ModifiedFiles -> Job -> IO ()
listen mC mM mJ = takeMVar mJ  >>= \job ->
  readMVar mC >>= \curFiles ->
  readMVar mM >>= \curMod ->
  updateFileStat mC mM curFiles curMod >>
  return ()

updateFileStat :: CurrentFiles -> ModifiedFiles -> [FileInfo] -> [FileInfo] -> IO ()
updateFileStat mC mM curFiles [] = return ()
updateFileStat mC mM curFiles curMod =
  swapMVar mC curFiles >>
  swapMVar mM curMod >>
  return()


initialize :: FileListenInfo -> IO (CurrentFiles, ModifiedFiles)
initialize fli =
  newMVar [] >>= \mC ->
  newMVar [] >>= \mM ->
  newEmptyMVar >>= \mJ ->
  (forever $ listen mC mM mJ) >>= \act ->
  forkIO act >>
  (forkIO $ schedule fli mJ) >>
  return (mC,mM)

schedule :: FileListenInfo -> Job -> IO()
schedule fileListenInfo mJ =
  putMVar mJ fileListenInfo >>
  threadDelay delay >>
  schedule fileListenInfo mJ >>
  return ()

