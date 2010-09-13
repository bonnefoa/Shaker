-- | Standard and simple actions
module Shaker.Action.Standard
 where

import Shaker.Type
import qualified Data.Map as M
import Control.Monad.Trans
import Control.Monad.Reader
import System.Directory

-- | Print the list of available actions
runHelp ::  Plugin
runHelp = do 
  commands <- asks commandMap 
  lift $ do   
  putStrLn "Following actions are available : "
  print $ M.keys commands
  putStrLn "use ~[actionName] for continuous launch"
 
-- | Print exit. The real exit management is made in conductor
runExit :: Plugin
runExit = lift $ putStrLn "Exiting"

-- | Print a begin action notification
runStartAction :: Plugin
runStartAction = lift $ 
  putStrLn "---------- Begin action -------------------------"

-- | Print an end action notification
runEndAction :: Plugin
runEndAction = lift $ 
  putStrLn "---------- End action ---------------------------"

-- | Clean action is responsible to delete directory containing temporary .o and .hi files 
runClean :: Plugin 
runClean = do
     toClean <- asks $ map cfCompileTarget . compileInputs
     lift$  mapM_ action toClean 
  where action toClean = do
                 ex <- doesDirectoryExist toClean
                 if ex then removeDirectoryRecursive toClean  
                       else putStrLn "" 

runInvalidAction :: Plugin 
runInvalidAction = lift $
  putStrLn "Action invalid, use help to display available actions"

