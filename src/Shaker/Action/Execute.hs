module Shaker.Action.Execute
 where

import Shaker.Type
import Shaker.SourceHelper
import GHC.Paths
import GHC
import Control.Monad.Reader

runExecute :: Plugin
runExecute = do
  cpList <- asks compileInputs 
  let cpIn = mergeCompileInputsSources cpList 
  lift $ runGhc (Just libdir) $ do 
            _ <- ghcCompile cpIn
            modSummaries <- getModuleGraph
            return ()
  return ()
