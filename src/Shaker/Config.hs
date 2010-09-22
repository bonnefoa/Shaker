-- | Contains the default configuration of shaker
module Shaker.Config
 where

import Shaker.Type
import Shaker.PluginConfig
import Shaker.Cli(InputState(..))
import Control.Concurrent

defaultInput ::ShakerInput  
defaultInput = ShakerInput {
  compileInputs = [defaultCompileInput],
  listenerInput = defaultListenerInput,
  pluginMap = defaultPluginMap,
  commandMap = defaultCommandMap
  ,argument = Nothing
  ,modifiedInfoFiles = []
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
  thread_listen <- newMVar [] :: IO ( ThreadIdList ) 
  thread_quit <- newMVar [] :: IO ( ThreadIdList )
  listen_token <- newEmptyMVar 
  quit_token <- newEmptyMVar  
  keyboard_token <- newEmptyMVar
  process_token <- newMVar 42 
  return ThreadData {
      keyboardToken = keyboard_token
      ,listenToken = listen_token
      ,quitToken = quit_token
      ,processToken = process_token
      ,threadIdListenList = thread_listen
      ,threadIdQuitList = thread_quit
    } 

defaultInputState :: IO InputState
defaultInputState = do
  inputMv <- newEmptyMVar 
  tokenMv <- newEmptyMVar  
  return InputState { input = inputMv, token =  tokenMv } 

