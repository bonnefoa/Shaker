-- | Conductor is responsible to control the command-line listener, 
-- the listener manager and the action to execute
module Shaker.Conductor(
 initThread
)
  where

import Shaker.Type
import Control.Monad
import Control.Concurrent
import Shaker.Listener
import Shaker.Cli
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Trans
 

type Shaker  = ReaderT ShakerInput 

-- | Initialize the master thread 
-- Once the master thread is finished, all input threads are killed
initThread :: InputState -> Shaker IO()
initThread inputState = do
  shakerInput <- ask
  procId <- lift $ forkIO $ forever (getInput shakerInput inputState)   
  mainThread inputState 
  lift $ killThread procId
 
-- | The main thread. 
-- Loop until a Quit action is called
mainThread :: InputState -> Shaker IO()
mainThread st@(InputState inputMv tokenMv) = do
  shakerInput <- ask
  _ <- lift $ tryPutMVar tokenMv 42
  cmd <- lift $ takeMVar inputMv
  lift $ executeCommand cmd  shakerInput
  case cmd of
       Command _ Quit -> return ()
       _ ->  mainThread st

-- | Continuously execute the given action until a keyboard input is done
listenManager :: IO() -> ShakerInput -> IO()
listenManager fun shakerInput = do
  -- Setup keyboard listener
  endToken <- newEmptyMVar 
  procCharListener <- forkIO $ getChar >>= putMVar endToken
  -- Setup source listener
  listenState <- initialize listenInput
  -- Run the action
  procId <- forkIO $ forever $ threadExecutor listenState fun
  _ <- readMVar endToken 
  mapM_ killThread  $  [procId,procCharListener] ++ threadIds listenState
  where listenInput = listenerInput shakerInput
  
-- | Execute the given action when the modified MVar is filled
threadExecutor :: ListenState -> IO() -> IO ThreadId
threadExecutor listenState fun = 
  takeMVar (modifiedFiles listenState) >> forkIO fun 

-- | Execute Given Command in a new thread
executeCommand :: Command -> ShakerInput -> IO()
executeCommand (Command OneShot act) shakerInput = executeAction act shakerInput 
executeCommand (Command Continuous act) shakerInput = listenManager ( executeAction act shakerInput) shakerInput

-- | Execute given action
executeAction :: Action -> ShakerInput -> IO()
executeAction act shakerInput = 
    case M.lookup act (pluginMap shakerInput) of
      Just action -> action shakerInput
      Nothing -> putStrLn $ "action "++ show act ++" is not registered"

