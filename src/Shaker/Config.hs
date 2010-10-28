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
  shakerCompileInputs = [mempty]
  ,shakerListenerInput= mempty
  ,shakerPluginMap = defaultPluginMap
  ,shakerCommandMap = defaultCommandMap
  ,shakerArgument = []
  ,shakerModifiedInfoFiles = []
  ,shakerThreadData = undefined
  ,shakerInputState = undefined
  ,shakerLocalBuildInfo = undefined
  }

defaultInputInitialized :: IO ShakerInput 
defaultInputInitialized = do 
  defThrdData <- defaultThreadData
  input_state <- defaultInputState 
  return defaultInput { 
    shakerThreadData = defThrdData 
    ,shakerInputState = input_state 
 }

defaultThreadData :: IO ThreadData 
defaultThreadData = do 
  thread_listen <- newMVar [] :: IO  ThreadIdList 
  thread_quit <- newMVar [] :: IO ThreadIdList 
  listen_token <- newEmptyMVar 
  quit_token <- newEmptyMVar  
  return ThreadData {
      threadDataListenToken = listen_token
      ,threadDataQuitToken = quit_token
      ,threadDataListenList = thread_listen
      ,threadDataQuitList = thread_quit
    } 

defaultInputState :: IO InputState
defaultInputState = do
  inputMv <- newEmptyMVar 
  tokenMv <- newEmptyMVar  
  return InputState { shakerInputStateCommand = inputMv, shakerInputStateToken =  tokenMv } 

