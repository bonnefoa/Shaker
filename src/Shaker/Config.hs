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
  }

defaultInputState :: IO InputState
defaultInputState = do
  inputMv <- newEmptyMVar 
  tokenMv <- newEmptyMVar  
  return InputState { input = inputMv, token =  tokenMv } 

