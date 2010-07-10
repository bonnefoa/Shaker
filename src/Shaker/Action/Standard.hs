module Shaker.Action.Standard
 where

import Shaker.Type
import qualified Data.Map as M
import Control.Monad.Trans
import Control.Monad.Reader

runHelp :: Plugin
runHelp = do 
  commands <- asks commandMap 
  lift $ do   
  putStrLn "Following actions are available : "
  print $ M.keys commands
  putStrLn "use ~[actionName] for continuous launch"

runExit :: Plugin
runExit = lift $ putStrLn "Exiting"

runStartAction :: Plugin
runStartAction = lift $ putStrLn ""

runEndAction :: Plugin
runEndAction = lift $ putStrLn "End action"
