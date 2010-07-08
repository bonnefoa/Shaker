module Shaker.Action.Clean
 where

import Shaker.Type
import System.Directory


runClean :: Plugin 
runClean shIn = do
  ex <- doesDirectoryExist toClean
  case ex of
     True -> removeDirectoryRecursive toClean
     False -> putStrLn "No target to remove" 
 where toClean = cfCompileTarget . compileInput $ shIn

