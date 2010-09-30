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
-- Once quit is called, all threads are killed
initThread :: Shaker IO()
initThread = do
  shIn <- ask
  input_action <- getInput 
  lift ( forkIO ( forever input_action) ) >>= addThreadIdToQuitMVar 
  let main_loop = runReaderT mainThread shIn 
  lift ( forkIO (forever main_loop) ) >>= addThreadIdToQuitMVar
  quit_token <- asks (quitToken . threadData)
  _ <- lift $ takeMVar quit_token
  cleanAllThreads 
 
-- | The main thread. 
mainThread :: Shaker IO()
mainThread = do
  (InputState inputMv tokenMv) <- asks inputState
  _ <- lift $ tryPutMVar tokenMv 42
  maybe_cmd <- lift $ takeMVar inputMv 
  executeCommand maybe_cmd

data ConductorData = ConductorData ListenState ([FileInfo] -> IO () )

initializeConductorData :: Shaker IO () -> Shaker IO ConductorData 
initializeConductorData fun = do
  shIn <- ask
  lstState <- initializeListener 
  mapM_ addThreadIdToListenMVar $ threadIds lstState 
  let theFun = \a -> runReaderT fun shIn {modifiedInfoFiles = a}
  return $ ConductorData lstState theFun
  
cleanAllThreads :: Shaker IO ()
cleanAllThreads = do 
  asks ( threadIdListenList . threadData ) >>= cleanThreads
  asks ( threadIdQuitList . threadData ) >>= cleanThreads

cleanThreads :: ThreadIdList -> Shaker IO()
cleanThreads thrdList = lift (readMVar thrdList)  >>= lift . mapM_ killThread 

-- | Execute the given action when the modified MVar is filled
threadExecutor :: ConductorData -> Shaker IO ()  
threadExecutor conductorData = do 
  shIn <- ask
  res <- lift $  handleContinuousInterrupt $ runReaderT (threadExecutor' conductorData) shIn
  when res $ threadExecutor conductorData
  
threadExecutor' :: ConductorData -> Shaker IO Bool
threadExecutor' (ConductorData listenState fun) = lift $ takeMVar (mvModifiedFiles listenState) >>= fun >> return True

-- | Execute Given Command in a new thread
executeCommand :: Maybe Command -> Shaker IO ()
executeCommand Nothing = executeAction [Action InvalidAction] 
executeCommand (Just (Command OneShot act_list)) = executeAction act_list 
executeCommand (Just (Command Continuous act)) = initializeConductorData ( executeAction act )  >>= threadExecutor 

-- | Execute given action
executeAction :: [Action] -> Shaker IO()
executeAction acts = do 
  shIn <- ask
  let allActs = runReaderT (mapM_ executeAction'  acts) shIn
  lift $ handleActionInterrupt allActs
  return () 

-- | Execute a single action with argument
executeAction' :: Action -> Shaker IO()
executeAction' (ActionWithArg actKey arg) = do 
  plMap <- asks pluginMap 
  local (\shIn -> shIn {argument = Just arg} ) $ fromJust $ actKey `M.lookup` plMap

-- | Execute a single action without argument
executeAction' (Action actKey) = do
  plMap <- asks pluginMap 
  fromJust $ actKey `M.lookup` plMap

-- * Handlers 

handleContinuousInterrupt :: IO Bool -> IO Bool
handleContinuousInterrupt = C.handle catchAll 
  where catchAll :: C.SomeException -> IO Bool
        catchAll e = putStrLn ("Shaker caught " ++ show e ) >>  return False

handleActionInterrupt :: IO() -> IO()
handleActionInterrupt =  C.handle catchAll
  where catchAll :: C.SomeException -> IO ()
        catchAll e = putStrLn ("Shaker caught " ++ show e ) >>  return ()

-- * Mvar with threadId list management

-- | Add the given threadId to the listener thread list
addThreadIdToListenMVar :: ThreadId -> Shaker IO()
addThreadIdToListenMVar thrdId = asks (threadIdListenList . threadData) >>= flip addThreadIdToMVar thrdId

-- | Add the given threadId to the quit thread list
addThreadIdToQuitMVar :: ThreadId -> Shaker IO()
addThreadIdToQuitMVar thrdId = asks (threadIdQuitList . threadData) >>= flip addThreadIdToMVar thrdId

-- | Add the given threadId to the mvar list
addThreadIdToMVar :: ThreadIdList -> ThreadId -> Shaker IO ()
addThreadIdToMVar thrdList thrId = lift $ modifyMVar_ thrdList (\b -> return $ thrId:b) 

