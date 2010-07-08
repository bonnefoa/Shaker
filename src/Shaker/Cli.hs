module Shaker.Cli(
  getInput
  ,listActions
)
 where

import Shaker.Parser
import Shaker.Type
import Control.Concurrent
import Control.Monad.Trans
import System.Console.Haskeline
import qualified Data.Map as M
import Data.List

-- | Listen to keyboard input and parse command
getInput :: ShakerInput -> InputState -> IO()
getInput shIn (InputState inputMv tokenMv) =runInputT (myDefaultSettings shIn) $ do
        _ <- lift $ takeMVar tokenMv 
        minput <- getInputLine "% "
        case minput of 
             Nothing -> return()
             Just str -> lift $ tryPutMVar inputMv (parseCommand shIn str) >> return() 

myDefaultSettings :: MonadIO m => ShakerInput-> Settings m
myDefaultSettings shIn = Settings {
  complete = completeAction shIn,
  historyFile = Nothing,
  autoAddHistory = True
}

completeAction :: Monad m => ShakerInput -> CompletionFunc m
completeAction shIn = completeWord (Just '\\') "\"'~" (listActions shIn)

listActions :: Monad m => ShakerInput -> String -> m [Completion]
listActions shIn = fun 
  where fun str = return $ filtered str
        cmdMap = commandMap shIn 
        filtered cliInput = map simpleCompletion $ filter (cliInput `isPrefixOf`) $ M.keys cmdMap

