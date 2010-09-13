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
  maybe_cmd <- lift $ takeMVar inputMv 
  continue <- executeCommand maybe_cmd
  if continue then mainThread st 
              else return ()

data ConductorData = ConductorData {
  coKillChannel :: MVar [ThreadId]
  ,coEndToken :: MVar Char
  ,coEndProcess :: MVar Int
  ,coListenState :: ListenState
  ,coFun :: [FileInfo] -> IO ()
 }

-- | Continuously execute the given action until a keyboard input is done
listenManager :: Shaker IO() -> Shaker IO()
listenManager fun = do
  conductorData <- initializeConductorData fun 
  lift $ action conductorData
  where action conductorData = do
         -- Setup keyboard listener
         forkIO (getChar >>= putMVar (coEndToken conductorData) ) >>= addThreadIdToMVar conductorData
         -- Run the action
         forkIO (forever $ threadExecutor conductorData) >>= addThreadIdToMVar conductorData
         _ <- readMVar (coEndToken conductorData)
         cleanThreads conductorData

initializeConductorData :: Shaker IO () -> Shaker IO ConductorData 
initializeConductorData fun = do
  shIn <- ask
  lstState <- initializeListener 
  killChannel <- lift $ newMVar [] 
  endToken <- lift newEmptyMVar 
  endProcess <- lift ( newMVar 42 :: IO ( MVar Int ) )
  let theFun = \a -> runReaderT fun shIn {modifiedInfoFiles = a}
  return $ ConductorData killChannel endToken endProcess lstState theFun
  
cleanThreads :: ConductorData -> IO()
cleanThreads (ConductorData chan _ _ lsState _) = do 
  lstChan <- readMVar chan
  mapM_ killThread $ lstChan ++ threadIds lsState

addThreadIdToMVar :: ConductorData -> ThreadId -> IO ()
addThreadIdToMVar conductorData thrId = modifyMVar_ (coKillChannel conductorData) (\b -> return $ thrId:b) 

-- | Execute the given action when the modified MVar is filled
threadExecutor :: ConductorData -> IO ()
threadExecutor cdtData@(ConductorData _ _ endProcess listenState fun) = do 
  modFiles <- takeMVar (mvModifiedFiles listenState)
  _ <- takeMVar endProcess
  forkIO (fun modFiles `C.finally` putMVar endProcess 42) >>= addThreadIdToMVar cdtData
  
-- | Execute Given Command in a new thread
executeCommand :: Maybe Command -> Shaker IO(Bool)
executeCommand Nothing = executeAction [Action InvalidAction] >> return True
executeCommand (Just (Command OneShot act_list)) 
  | (Action Quit) `elem` act_list = return False 
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

