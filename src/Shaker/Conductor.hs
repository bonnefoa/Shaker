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

initThread = do
  inputMv <-  newEmptyMVar 
  tokenMv <-  newEmptyMVar 
  procId <- (forkIO $ forever (getInput inputMv tokenMv) )  
  mainThread InputState { 
      input = inputMv,
      token =  tokenMv
  }
  killThread procId

-- mainThread :: InputShaker IO()
mainThread st@(InputState input token ) = do
  tryPutMVar token 42
  cmd <- takeMVar input
  executeCommand cmd
  case cmd of
       Command _ Quit -> return ()
       _ ->  mainThread st

leon :: [ThreadId] -> IO()
leon = mapM_ killThread

listenManager fun = do
  endToken <- newEmptyMVar 
  procCharListener <- forkIO ( charListen endToken) 
  listenState <- listenProjectFiles 
  procId <- forkIO (forever (threadExecutor listenState fun)) 
  readMVar endToken 
  leon $  [procId,procCharListener] ++ getListenThreads listenState
  
threadExecutor (ListenState _ modF  _) fun = 
  takeMVar modF >> fun 
  
charListen endToken = getChar >>= putMVar endToken

-- ^ Listen to keyboard input and parse command
getInput inputMv token = do
 takeMVar token 
 putStr ">" 
 inputt <- getLine
 tryPutMVar inputMv (parseCommand input)
 return ()

executeCommand (Command OneShot act) = executeAction act
executeCommand (Command Continuous act) = listenManager $ executeAction act

executeAction Compile = runCompileProject >> return()
executeAction Quit = putStrLn "Exiting"
executeAction _ = runHelp
   
