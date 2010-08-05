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

launchFunction :: (Maybe String) -> Plugin 
launchFunction Nothing = lift $ putStrLn "No action to execute. Give an argument of ModuleName.functionName. The function should be of type IO()"
launchFunction (Just actStr) = lift $ putStrLn "Ga "
  where (mod, fun) = parseModuleAndAction actStr

parseModuleAndAction :: String -> (String,String)
parseModuleAndAction actStr = (moduleStr, functionStr)
  where (functionStr, moduleStr) = ( \(a,b) -> (a,reverse . tail $ b) ) . span (/= '.') . reverse $ actStr
