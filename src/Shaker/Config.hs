module Shaker.Config
 where

import Shaker.Type
import DynFlags
import GHC
import Shaker.Conductor
import qualified Data.Map as M
import Shaker.Action.Compile
import Shaker.Action.Standard

defaultInput ::ShakerInput  
defaultInput = ShakerInput {
  compileInput = CompileInput (\a-> a  {
    importPaths = ["src/","testsuite/tests/"], 
    verbosity = 1, 
    objectDir = Just "target",
    hiDir = Just "target",
    packageFlags = [ExposePackage "ghc"]
  }),
  listenerInput = ListenerInput {
    fileListenInfo= FileListenInfo "." [] [".*\\.hs$"],
    delay = 2*10^6
    },
  pluginMap = defaultPluginMap
  }

defaultPluginMap :: PluginMap
defaultPluginMap = M.fromList list
  where list = [
                (Compile,runCompile),
                (Help,runHelp),
                (Quit,runExit)
              ]
