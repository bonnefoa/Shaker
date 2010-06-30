module Shaker.Conductor
  where

import Shaker.Type
import Shaker.Parser
import Shaker.CompileAction
import Shaker.LoadAction
import Shaker.HelpAction

--parseAndExecute :: String -> IO()
parseAndExecute cmd = executeCommand act 
 where (Command tp act) = parseCommand cmd


executeCommand Compile = runCompileProject

