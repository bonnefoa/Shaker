module Shaker.Cli
 where

import Shaker.Parser
import Shaker.Type
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar
import System.Console.Haskeline
import System.Console.Haskeline.Completion

-- | Listen to keyboard input and parse command
getInput :: ShakerInput -> InputState -> IO()
getInput shIn (InputState inputMv token) =runInputT myDefaultSettings action
 where  action::InputT IO()
        action  = do
        lift $ takeMVar token 
        minput <- getInputLine "% "
        case minput of 
             Nothing -> return()
             Just str -> lift $ tryPutMVar inputMv (parseCommand shIn str) >> return() 

myDefaultSettings :: MonadIO m => Settings m
myDefaultSettings = Settings {
  complete = completeAction ,
  historyFile = Nothing,
  autoAddHistory = True
}

completeAction :: Monad m => CompletionFunc m
completeAction = completeWord (Just '\\') "\"'" listActions

listActions :: Monad m => String -> m [Completion]
listActions  str = return $  [simpleCompletion "Quit"]

