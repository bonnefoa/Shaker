module Shaker.Action.Clean
 where

import Shaker.Type
import System.Directory
import Control.Monad.Trans 


runClean :: Plugin 
runClean shIn = lift action
 where toClean = cfCompileTarget . compileInput $ shIn
       action = do
                   ex <- doesDirectoryExist toClean
                   case ex of
                     True -> removeDirectoryRecursive toClean
                     False -> putStrLn "No target to remove" 
