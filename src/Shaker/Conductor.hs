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

initThread = 
  newEmptyMVar >>= \mv ->
  forever (forkIO (getInput mv ) )  >> 
  mainThread mv

mainThread mv = 
  takeMVar mv >>= \cmd -> 
  executeCommand cmd >>
  case cmd of 
       (Command _ Quit) -> return "Exiting"
       _ -> mainThread mv


getInput :: MVar Command -> IO()
getInput mv = 
 putStr ">" >>
 getLine >>= \input ->
 tryPutMVar mv (parseCommand input) >>
 return ()

executeCommand (Command _ Compile) = runCompileProject >>= \res ->
  putStrLn $ "project compiled with modules " ++ show res 
executeCommand _ = runHelp

