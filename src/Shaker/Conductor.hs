module Shaker.Conductor
  where

import Shaker.Type

parseAndExecute :: String -> IO()
parseAndExecute cmd = putStrLn "ga"
 where (Command tp act) =  read cmd

executeAction :: Command -> IO()
executeAction (Command tp act) = putStrLn "ga"



