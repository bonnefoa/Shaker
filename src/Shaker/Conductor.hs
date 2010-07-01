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
  newEmptyMVar >>= \token ->
  (forkIO $ forever (getInput mv token) )  >>= \procId -> 
  mainThread mv procId token

mainThread mv procId token = 
  tryPutMVar token "42">>
  takeMVar mv >>= \cmd -> 
  executeCommand cmd >>
  case cmd of 
       (Command _ Quit) ->  killThread procId
       _ -> mainThread mv procId token

getInput mv token = 
 takeMVar token >>
 putStr ">" >>
 getLine >>= \input ->
 tryPutMVar mv (parseCommand input) >>
 return ()

executeCommand (Command _ Compile) = runCompileProject >>= \res ->
  putStrLn $ "project compiled with modules " ++ show res 
executeCommand (Command _ Quit) = putStrLn "Exiting"
executeCommand _ = runHelp

