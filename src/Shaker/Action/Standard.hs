module Shaker.Action.Standard
 where

import Shaker.Type
import qualified Data.Map as M
import Control.Monad.Trans

runHelp :: Plugin
runHelp shakerInput = lift $ do 
  putStrLn "Following actions are available : "
  print $ M.keys $ commandMap shakerInput
  putStrLn "use ~[actionName] for continuous launch"

runExit :: Plugin
runExit _ = lift $ putStrLn "Exiting"

runStartAction :: Plugin
runStartAction _ = lift $ putStrLn ""

runEndAction :: Plugin
runEndAction _ = lift $ putStrLn "End action"
