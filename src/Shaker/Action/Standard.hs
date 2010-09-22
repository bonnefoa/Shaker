-- | Standard and simple actions
module Shaker.Action.Standard
 where

import Shaker.Type
import qualified Data.Map as M
import Control.Monad.Trans
import Control.Monad.Reader
import System.Directory
import Control.Concurrent

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
runExit = do
  lift $ putStrLn "Exiting"
  quit_token <- asks (quitToken . threadData)
  lift $ putMVar quit_token 42

-- | Print a begin action notification
runStartAction :: Plugin
runStartAction = lift $ 
  putStrLn "---------- Begin action -------------------------"

runEmpty :: Plugin
runEmpty = return ()

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
runInvalidAction = lift $ putStrLn "Invalid action,  use help to display available actions"

