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

import Shaker.Parser
import Shaker.Type
import Control.Concurrent
import Control.Monad.Trans
import System.Console.Haskeline
import qualified Data.Map as M
import Data.List
import Control.Monad.Reader
 
-- | The input mvar is used to push the parsed command
type Input = MVar Command
-- | Token is used to manage the token between action executor and command-line listener
type Token = MVar Int

data InputState = InputState {  
  input :: Input,
  token :: Token
}

-- | Listen to keyboard input and parse command
getInput :: InputState -> Shaker IO()
getInput inSt = do
        shIn <- ask 
        lift $ runInputT (myDefaultSettings shIn) $ processInput shIn inSt

-- | Execute the entered command 
processInput :: ShakerInput ->  InputState -> InputT IO()
processInput shIn inp@(InputState inputMv tokenMv) = do
  _ <- lift $ takeMVar tokenMv 
  minput <- getInputLine "% "
  case minput of 
     Nothing -> return()
     Just str -> lift $ processInput' shIn str inp

processInput' :: ShakerInput -> String -> InputState ->IO()
processInput' shIn str (InputState inputMv tokenMv) = do
  case parseCommand shIn str of
       Just command -> tryPutMVar inputMv (command) >> return()
       Nothing -> return() 

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
listActions shIn str = return $ autocompleteFunction (commandMap shIn) str

autocompleteFunction :: CommandMap  -> String -> [Completion]
autocompleteFunction cmdMap [] = map simpleCompletion $ M.keys cmdMap
autocompleteFunction cmdMap cliInput = map simpleCompletion  compleListProp
  where inpWords = words cliInput
        lastWord = last inpWords 
        listProp = filter (lastWord `isPrefixOf`) $ M.keys cmdMap
        commonPref = unwords (init inpWords)
        compleListProp = trimList $ map  (\a -> commonPref ++ " " ++ a) listProp

trimList :: [String] -> [String]
trimList = map (dropWhile (== ' '))

