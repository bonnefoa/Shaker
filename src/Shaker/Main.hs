module Shaker.Main
 where

import Shaker.Conductor
import Shaker.Type
import Control.Concurrent
import Control.Concurrent.MVar

main = do
  inputMv <-  newEmptyMVar 
  tokenMv <-  newEmptyMVar 
  initThread  InputState { 
      input = inputMv,
      token =  tokenMv
  }  ListenerInput {
    fileListenInfo= FileListenInfo "." [] [".*\\.hs$"],
    delay = 2*10^6
  }

