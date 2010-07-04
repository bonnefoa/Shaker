module Shaker.Conductor
  where

import Shaker.Type
import Shaker.Parser
import Shaker.Action.Compile
import Shaker.Action.Load
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Shaker.Listener
import Shaker.Cli
import Control.Monad.State
import qualified Data.Map as M
 
-- | Initialize the master thread 
-- Once the master thread is finished, all input threads are killed
initThread :: InputState -> ShakerInput -> IO()
initThread inputState shakerInput = do
  procId <- forkIO $ forever (getInput shakerInput inputState)   
  mainThread inputState shakerInput 
  killThread procId
 
-- | The main thread. 
-- Loop until a Quit action is called
mainThread :: InputState -> ShakerInput -> IO()
mainThread st@(InputState input token) shakerInput = do
  tryPutMVar token 42
  cmd <- takeMVar input
  executeCommand cmd shakerInput
  case cmd of
       Command _ Quit -> return ()
       _ ->  mainThread st shakerInput 

-- | Continuously execute the given action until a keyboard input is done
listenManager :: IO() -> ShakerInput -> IO()
listenManager fun shakerInput = do
  -- Setup keyboard listener
  endToken <- newEmptyMVar 
  procCharListener <- forkIO $ getChar >>= putMVar endToken
  -- Setup source listener
  listenState <- listenProjectFiles listenInput
  -- Run the action
  procId <- forkIO $ forever $ threadExecutor listenState fun
  readMVar endToken 
  mapM_ killThread  $  [procId,procCharListener] ++ getListenThreads listenState
  where listenInput = getListenerInput shakerInput
  
-- | Execute the given action when the modified MVar is filled
threadExecutor :: ListenState -> IO() -> IO(ThreadId)
threadExecutor (ListenState _ modF  _) fun = 
  takeMVar modF >> forkIO fun 

-- | Execute Given Command in a new thread
executeCommand :: Command -> ShakerInput -> IO()
executeCommand (Command OneShot act) shakerInput = executeAction act shakerInput 
executeCommand (Command Continuous act) shakerInput = listenManager ( executeAction act shakerInput) shakerInput

-- | Execute given action
executeAction :: Action -> ShakerInput -> IO()
executeAction act shakerInput = 
    case M.lookup act (getPluginMap shakerInput) of
      Just action -> action shakerInput
      Nothing -> putStrLn $ "action "++ show act ++" is not registered"
                                     


