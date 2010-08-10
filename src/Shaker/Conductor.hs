-- | Conductor is responsible to control the command-line listener, 
-- the listener manager and the action to execute
module Shaker.Conductor(
 initThread,
 executeCommand
)
  where

import Shaker.Type
import Control.Monad
import Control.Concurrent
import Shaker.Listener
import Shaker.Cli
import qualified Data.Map as M
import Control.Monad.Reader
import Data.Maybe
import qualified Control.Exception as C
 
-- | Initialize the master thread 
-- Once the master thread is finished, all input threads are killed
initThread :: InputState -> Shaker IO()
initThread inputState = do
  act <- asks $ runReaderT (getInput inputState) 
  procId <- lift $ forkIO $ forever act
  mainThread inputState 
  lift $ killThread procId
 
-- | The main thread. 
-- Loop until a Quit action is called
mainThread :: InputState -> Shaker IO()
mainThread st@(InputState inputMv tokenMv) = do
  _ <- lift $ tryPutMVar tokenMv 42
  cmd <- lift $ takeMVar inputMv
  executeCommand cmd  
  case cmd of
       Command _ [Action Quit] -> return ()
       _ ->  mainThread st

-- | Continuously execute the given action until a keyboard input is done
listenManager :: Shaker IO() -> Shaker IO()
listenManager fun = do
  shIn <- ask 
  lift $ action shIn 
  where action shIn = do
          -- Setup keyboard listener
          endToken <- newEmptyMVar 
          endProcess <- newMVar 42 :: IO ( MVar Int )
          procCharListener <- forkIO $ getChar >>= putMVar endToken
          -- Setup source listener
          listenState <- initialize (listenerInput shIn)
          -- Run the action
          procId <-  forkIO $ forever $ threadExecutor listenState endProcess (runReaderT fun shIn)
          _ <- readMVar endToken 
          mapM_ killThread  $  [procId,procCharListener] ++ threadIds listenState
  
-- | Execute the given action when the modified MVar is filled
threadExecutor :: ListenState -> MVar Int -> IO() -> IO ThreadId
threadExecutor listenState endProcess fun = do 
  _ <- takeMVar (modifiedFiles listenState)
  _ <- takeMVar endProcess
  forkIO $ fun `C.finally` putMVar endProcess 42
  
-- | Execute Given Command in a new thread
executeCommand :: Command -> Shaker IO()
executeCommand (Command OneShot act) = executeAction act 
executeCommand (Command Continuous act) = listenManager ( executeAction act ) >> return () 

-- | Execute given action
executeAction :: [Action] -> Shaker IO()
executeAction acts = do
   mapM_ executeAction' acts 
   return () 

executeAction' :: Action -> Shaker IO()
executeAction' (ActionWithArg act arg) = do 
  plMap<- asks pluginMap 
  local (\shIn -> shIn {argument = Just arg} ) $ fromJust $ act `M.lookup` plMap
executeAction' (Action act) = do
  plMap <- asks pluginMap 
  fromJust $ act `M.lookup` plMap

