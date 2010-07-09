module Shaker.Action.Load
 where

{-
  - import Language.Haskell.Interpreter
          
runLoadFiles :: [FilePath] -> IO (Either InterpreterError () )
runLoadFiles = runInterpreter . loadFiles 

loadFiles :: [FilePath] -> Interpreter()
loadFiles files = 
  set [searchPath := ["./src/","./testsuite/tests"] ] >>
  loadModules files >>
  getLoadedModules >>= liftIO . print

-}
