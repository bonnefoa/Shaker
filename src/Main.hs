module Main
 where

import Shaker.Conductor
import Shaker.Type
import Shaker.Config
import Control.Concurrent
import Control.Concurrent.MVar

main = do
  inputMv <- newEmptyMVar 
  tokenMv <- newEmptyMVar  
  let inputState = InputState { input = inputMv, token =  tokenMv } in
        initThread inputState defaultInput 

