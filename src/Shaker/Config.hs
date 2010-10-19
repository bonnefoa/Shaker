-- | Contains the default configuration of shaker
module Shaker.Config
 where

import Data.Monoid

import Shaker.Type
import Shaker.PluginConfig
import Shaker.Cli(InputState(..))
import Control.Concurrent

defaultInput ::ShakerInput  
defaultInput = ShakerInput {
  compileInputs = [mempty]
  ,listenerInput = mempty
  ,pluginMap = defaultPluginMap
  ,commandMap = defaultCommandMap
  ,argument = []
  ,modifiedInfoFiles = []
  ,threadData = undefined
  ,inputState = undefined
  }

defaultInputInitialized :: IO ShakerInput 
defaultInputInitialized = do 
  defThrdData <- defaultThreadData
  input_state <- defaultInputState 
  return defaultInput { 
    threadData = defThrdData 
    ,inputState = input_state 
 }

defaultThreadData :: IO ThreadData 
defaultThreadData = do 
  thread_listen <- newMVar [] :: IO  ThreadIdList 
  thread_quit <- newMVar [] :: IO ThreadIdList 
  listen_token <- newEmptyMVar 
  quit_token <- newEmptyMVar  
  return ThreadData {
      listenToken = listen_token
      ,quitToken = quit_token
      ,threadIdListenList = thread_listen
      ,threadIdQuitList = thread_quit
    } 

defaultInputState :: IO InputState
defaultInputState = do
  inputMv <- newEmptyMVar 
  tokenMv <- newEmptyMVar  
  return InputState { inputStateCommand = inputMv, inputStateToken =  tokenMv } 

