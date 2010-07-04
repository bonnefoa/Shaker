module Shaker.Conductor
  where

import Shaker.Type
import Shaker.Parser
import Shaker.CompileAction
import Shaker.LoadAction
import Shaker.HelpAction
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Shaker.Listener
import Control.Monad.State

initThread :: InputState -> ListenerInput -> IO()
initThread inputState listenInput= do
  procId <- forkIO $ forever (getInput inputState)   
  mainThread inputState listenInput
  killThread procId

-- mainThread :: InputShaker IO()
mainThread st@(InputState input token) listenInput = do
  tryPutMVar token 42
  cmd <- takeMVar input
  executeCommand cmd listenInput
  case cmd of
       Command _ Quit -> return ()
       _ ->  mainThread st listenInput

leon :: [ThreadId] -> IO()
leon = mapM_ killThread

listenManager fun listenInput = do
  endToken <- newEmptyMVar 
  procCharListener <- forkIO ( charListen endToken) 
  listenState <- listenProjectFiles listenInput
  procId <- forkIO (forever (threadExecutor listenState fun)) 
  readMVar endToken 
  leon $  [procId,procCharListener] ++ getListenThreads listenState
  
threadExecutor (ListenState _ modF  _) fun = 
  takeMVar modF >> forkIO fun 
  
charListen endToken = getChar >>= putMVar endToken

-- ^ Listen to keyboard input and parse command
getInput (InputState inputMv token) = do
 takeMVar token 
 putStr ">" 
 input <- getLine
 tryPutMVar inputMv (parseCommand input)
 return () 

executeCommand (Command OneShot act) _ = executeAction act
executeCommand (Command Continuous act) listenInput = listenManager ( executeAction act) listenInput

executeAction Compile = runCompileProject >> return()
executeAction Quit = putStrLn "Exiting"
executeAction _ = runHelp
   

