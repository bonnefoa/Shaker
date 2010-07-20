module Shaker.Action.Clean
 where

import Shaker.Type
import System.Directory
import Control.Monad.Trans 
import Control.Monad.Reader

runClean :: Plugin 
runClean = do
       toClean <- asks $ map cfCompileTarget . compileInputs
       lift$  mapM_ action toClean 
    where action toClean = do
                   ex <- doesDirectoryExist toClean
                   if ex then removeDirectoryRecursive toClean  
                         else putStrLn "" 
