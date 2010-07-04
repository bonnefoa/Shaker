module Shaker.Config
 where

import Shaker.Type
import DynFlags
import GHC
import Shaker.Conductor
import qualified Data.Map as M
import Shaker.Action.Compile

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
defaultPluginMap = M.insert Compile runCompile M.empty
