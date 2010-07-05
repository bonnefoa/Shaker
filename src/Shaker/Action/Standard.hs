module Shaker.Action.Standard
 where

import Shaker.Type
import qualified Data.Map as M

runHelp :: Plugin
runHelp shakerInput = do 
  putStrLn "Following actions are available : "
  print $ M.keys $ getCommandMap shakerInput
  putStrLn "use ~[actionName] for continuous launch"

runExit :: Plugin
runExit _ = putStrLn "Exiting"

