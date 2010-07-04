module Shaker.Cli
 where

import Shaker.Parser
import Shaker.Type
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar
import System.Console.Haskeline

-- | Listen to keyboard input and parse command
getInput :: InputState -> IO()
getInput (InputState inputMv token) =runInputT defaultSettings action
 where  action::InputT IO()
        action  = do
        lift $ takeMVar token 
        minput <- getInputLine "% "
        case minput of 
             Nothing -> return()
             Just str -> lift $ tryPutMVar inputMv (parseCommand str) >> return() 

 
