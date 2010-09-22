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
  act <- getInput 
  procId <- lift $ forkIO $ forever act
  mainThread 
  lift $ killThread procId
 
-- | The main thread. 
-- Loop until a Quit action is called
mainThread :: Shaker IO()
mainThread = do
  (InputState inputMv tokenMv) <- asks inputState
  _ <- lift $ tryPutMVar tokenMv 42
  maybe_cmd <- lift $ takeMVar inputMv 
  continue <- executeCommand maybe_cmd
  when continue $ mainThread 

data ConductorData = ConductorData {
  coKillChannel :: MVar [ThreadId]
  ,coEndToken :: MVar Char -- For keyboard listen
--  ,coEndProcess :: MVar Int -- for listen process end
  ,coListenState :: ListenState
  ,coFun :: [FileInfo] -> IO ()
 }

-- | Continuously execute the given action until a keyboard input is done
listenManager :: Shaker IO() -> Shaker IO()
listenManager fun = do
  conductorData <- initializeConductorData fun 
  shIn <- ask
  let action = runReaderT (threadExecutor conductorData) shIn
  -- Setup keyboard listener
  lift ( forkIO (getChar >>= putMVar (coEndToken conductorData) ) ) >>= addThreadIdToMVar conductorData
  -- Run the action
  lift ( forkIO (forever action ) ) >>= addThreadIdToMVar conductorData
  _ <- lift $ readMVar (coEndToken conductorData)
  cleanThreads conductorData

initializeConductorData :: Shaker IO () -> Shaker IO ConductorData 
initializeConductorData fun = do
  shIn <- ask
  lstState <- initializeListener 
  killChannel <- lift $ newMVar [] 
  endToken <- lift newEmptyMVar 
  let theFun = \a -> runReaderT fun shIn {modifiedInfoFiles = a}
  return $ ConductorData killChannel endToken lstState theFun
  
cleanThreads :: ConductorData -> Shaker IO()
cleanThreads (ConductorData chan _ lsState _) = do 
  lstChan <- lift $ readMVar chan
  lift $ mapM_ killThread $ lstChan ++ threadIds lsState

addThreadIdToMVar :: ConductorData -> ThreadId -> Shaker IO ()
addThreadIdToMVar conductorData thrId = lift $ modifyMVar_ (coKillChannel conductorData) (\b -> return $ thrId:b) 

-- | Execute the given action when the modified MVar is filled
threadExecutor :: ConductorData -> Shaker IO ()
threadExecutor cdtData@(ConductorData _ _ listenState fun) = do 
  process_token <- asks (processToken . threadData ) 
  modFiles <- lift $ takeMVar (mvModifiedFiles listenState)
  _ <- lift $ takeMVar process_token 
  procId <- lift $ forkIO (fun modFiles `C.finally` putMVar process_token 42) 
  addThreadIdToMVar cdtData procId
  
-- | Execute Given Command in a new thread
executeCommand :: Maybe Command -> Shaker IO Bool
executeCommand Nothing = executeAction [Action InvalidAction] >> return True
executeCommand (Just (Command OneShot act_list)) 
  | Action Quit `elem` act_list = return False 
  | otherwise = executeAction act_list >> return True
executeCommand (Just (Command Continuous act)) = listenManager ( executeAction act ) >> return True

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

