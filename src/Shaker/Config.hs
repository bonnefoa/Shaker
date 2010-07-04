module Shaker.Config
 where

import Shaker.Type
import DynFlags
import GHC
import Shaker.Conductor

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
    }
  }
