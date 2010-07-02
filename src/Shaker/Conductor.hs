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

initThread = 
  newEmptyMVar >>= \inputMv ->
  newEmptyMVar >>= \tokenMv ->
  (forkIO $ forever (getInput inputMv tokenMv) )  >>= \procId -> 
  mainThread InputState { 
      input = inputMv,
      token =  tokenMv,
      threadCli = procId
  } >>= \inputState ->
  killThreads inputState

-- mainThread :: InputShaker IO()
mainThread st@(InputState input token threadCli) = 
  tryPutMVar token 42 >>
  takeMVar input >>= \cmd -> 
  executeCommand cmd >>
  case cmd of
       Command _ Quit -> return st
       _ ->  mainThread st

-- killThreads:: InputShaker IO()
killThreads (InputState _ _ threadCli)= 
  killThread threadCli   

killListenThread (ListenState _ _ threadListen threadSchedule)= 
  killThread threadListen >>
  killThread threadSchedule

-- listenManager :: ([FileInfo]-> IO()) -> IO() 
listenManager fun = newEmptyMVar >>= \endToken ->
  forkIO ( charListen endToken) >>
  listenProjectFiles >>= \listenState ->
  forkIO (forever (threadExecutor listenState fun)) >>= \procId ->
  readMVar endToken >>
  killListenThread listenState >>
  killThread procId
  
--threadExecutor :: ListenState -> (->IO()) -> IO ()
threadExecutor (ListenState _ modF _ _) fun = 
  takeMVar modF >>= \files ->
  fun 
  

charListen endToken = getChar >>= putMVar endToken

-- ^ Listen to keyboard input and parse command
getInput inputMv token = 
 takeMVar token >>
 putStr ">" >>
 getLine >>= \input ->
 tryPutMVar inputMv (parseCommand input) >>
 return ()

executeCommand (Command OneShot act) = executeAction act
executeCommand (Command Continuous act) = executeAction act

executeAction Compile = runCompileProject >> return()
executeAction Quit = putStrLn "Exiting"
executeAction _ = runHelp
   
