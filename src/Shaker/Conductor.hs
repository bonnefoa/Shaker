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
initThread :: Shaker IO()
initThread = do
  input_action <- getInput 
  lift ( forkIO ( forever input_action) ) >>= addThreadIdToQuitMVar 
  shIn <- ask
  let main_loop = runReaderT mainThread shIn 
  lift ( forkIO (forever main_loop) ) >>= addThreadIdToQuitMVar
  quit_token <- asks (quitToken . threadData)
  _ <- lift $ takeMVar quit_token
  cleanAllThreads 
 
-- | The main thread. 
-- Loop until a Quit action is called
mainThread :: Shaker IO()
mainThread = do
  (InputState inputMv tokenMv) <- asks inputState
  _ <- lift $ tryPutMVar tokenMv 42
  maybe_cmd <- lift $ takeMVar inputMv 
  executeCommand maybe_cmd

data ConductorData = ConductorData {
  coListenState :: ListenState
  ,coFun :: [FileInfo] -> IO ()
 }

-- | Continuously execute the given action until a keyboard input is done
listenManager :: Shaker IO() -> Shaker IO()
listenManager fun = do
  conductorData <- initializeConductorData fun 
  shIn <- ask
  keyboard_token <- asks (keyboardToken . threadData)
  let action = runReaderT (threadExecutor conductorData) shIn
  -- Setup keyboard listener
  lift ( forkIO (getChar >>= putMVar keyboard_token ) ) >>= addThreadIdToListenMVar 
  -- Run the action
  lift ( forkIO (forever action ) ) >>= addThreadIdToListenMVar
  _ <- lift $ takeMVar keyboard_token 
  cleanListenerThreads 

initializeConductorData :: Shaker IO () -> Shaker IO ConductorData 
initializeConductorData fun = do
  shIn <- ask
  lstState <- initializeListener 
  mapM_ addThreadIdToListenMVar $ threadIds lstState 
  let theFun = \a -> runReaderT fun shIn {modifiedInfoFiles = a}
  return $ ConductorData lstState theFun
  
cleanListenerThreads :: Shaker IO ()
cleanListenerThreads = asks ( threadIdListenList . threadData ) >>= cleanThreads

cleanAllThreads :: Shaker IO ()
cleanAllThreads = do 
  asks ( threadIdListenList . threadData ) >>= cleanThreads
  asks ( threadIdQuitList . threadData ) >>= cleanThreads

cleanThreads :: ThreadIdList -> Shaker IO()
cleanThreads thrdList = lift (readMVar thrdList)  >>= lift . mapM_ killThread 

-- | Add the given threadId to the listener thread list
addThreadIdToListenMVar :: ThreadId -> Shaker IO()
addThreadIdToListenMVar thrdId = asks (threadIdListenList . threadData) >>= flip addThreadIdToMVar thrdId

-- | Add the given threadId to the quit thread list
addThreadIdToQuitMVar :: ThreadId -> Shaker IO()
addThreadIdToQuitMVar thrdId = asks (threadIdQuitList . threadData) >>= flip addThreadIdToMVar thrdId

-- | Add the given threadId to the mvar list
addThreadIdToMVar :: ThreadIdList -> ThreadId -> Shaker IO ()
addThreadIdToMVar thrdList thrId = lift $ modifyMVar_ thrdList (\b -> return $ thrId:b) 

-- | Execute the given action when the modified MVar is filled
threadExecutor :: ConductorData -> Shaker IO ()
threadExecutor (ConductorData listenState fun) = do 
  process_token <- asks (processToken . threadData ) 
  modFiles <- lift $ takeMVar (mvModifiedFiles listenState)
  _ <- lift $ takeMVar process_token 
  procId <- lift $ forkIO (fun modFiles `C.finally` putMVar process_token 42) 
  addThreadIdToListenMVar procId
  
-- | Execute Given Command in a new thread
executeCommand :: Maybe Command -> Shaker IO ()
executeCommand Nothing = executeAction [Action InvalidAction] 
executeCommand (Just (Command OneShot act_list)) = executeAction act_list 
executeCommand (Just (Command Continuous act)) = listenManager ( executeAction act ) 

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

