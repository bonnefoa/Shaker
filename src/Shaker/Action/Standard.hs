module Shaker.Action.Standard
 where

import Shaker.Type

runHelp :: Plugin
runHelp _ = putStrLn "No help for now, but the answer should be 42"

runExit ::Plugin
runExit _ = putStrLn "Exiting"

