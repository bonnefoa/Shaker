module Shaker.Action.Clean
 where

import Shaker.Type
import System.Directory
import Control.Monad.Trans 
import Control.Monad.Reader

runClean :: Plugin 
runClean = do
       toClean <- asks $ cfCompileTarget . compileInput 
       lift$  action toClean 
    where action toClean = do
                   ex <- doesDirectoryExist toClean
                   case ex of
                     True -> removeDirectoryRecursive toClean
                     False -> putStrLn "No target to remove" 
