-- | Command line manager
-- This manager will listen to the standard input as soon as the MVar token is filled.
-- Then, it will fill another MVar (input) with the parsed command.
-- Autocompletion is supported throught haskeline configuration.
module Shaker.Cli(
  getInput
  ,listActions
  ,InputState(..)
)
 where

import Control.Concurrent
import Control.Monad.Reader
import Data.Char
import Data.List
import qualified Data.Map as M
import Shaker.CommonUtil
import Shaker.Parser
import Shaker.Type
import System.Console.Haskeline

-- | Listen to keyboard input and parse command
getInput :: Shaker IO( IO() )
getInput = do
  shIn <- ask 
  return $ runInputT (myDefaultSettings shIn) $ withInterrupt $ processInput shIn 

-- | Execute the entered command 
processInput :: ShakerInput -> InputT IO()
processInput shIn  = do
  let (InputState inputMv tokenMv) = shakerInputState shIn
  _ <- lift $ takeMVar tokenMv 
  minput <-  handleInterrupt (return (Just "quit"))
               $ getInputLine "% "
  case minput of 
     Nothing -> lift $ tryPutMVar inputMv (Just exitCommand ) >> return () 
     Just str -> either error_action normal_action (parseCommand str shIn)
                 where error_action err = lift $ print err >> tryPutMVar inputMv Nothing >> return()
                       normal_action val = lift $ tryPutMVar inputMv (Just val) >> return()

-- * Auto-completion management 

-- | Settings for haskeline
myDefaultSettings :: MonadIO m => ShakerInput-> Settings m
myDefaultSettings shIn = Settings {
  complete = completeAction shIn,
  historyFile = Just ".haskelineHistory",
  autoAddHistory = True
}

completeAction :: Monad m => ShakerInput -> CompletionFunc m
completeAction shIn = completeWord (Just '\\') "\"'~" (listActions shIn)

listActions :: Monad m => ShakerInput -> String -> m [Completion]
listActions shIn str = return $ autocompleteFunction (shakerCommandMap shIn) str

autocompleteFunction :: CommandMap  -> String -> [Completion]
autocompleteFunction cmdMap [] = map simpleCompletion $ M.keys cmdMap
autocompleteFunction cmdMap cliInput = map simpleCompletion  compleListProp
  where inpWords = (words . map toLower) cliInput
        lastWord = last inpWords 
        listProp = filter (lastWord `isPrefixOf`) $ M.keys cmdMap
        commonPref = unwords (init inpWords)
        compleListProp = trimList $ map  (\a -> commonPref ++ " " ++ a) listProp


