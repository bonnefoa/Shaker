-- | Contains the default configuration of shaker
module Shaker.Config
 where

import Shaker.Type
import DynFlags
import qualified Data.Map as M
import Shaker.Action.Compile
import Shaker.Action.Standard

defaultInput ::ShakerInput  
defaultInput = ShakerInput {
  compileInput = defaultCompileInput,
  listenerInput = defaultListenerInput,
  pluginMap = defaultPluginMap,
  commandMap = defaultCommandMap
  }

-- | Default compilation argument.
-- Wall is activated by default
defaultCompileInput :: CompileInput
defaultCompileInput = CompileInput  defaultCompileFlags  "-Wall"

-- | default dynamics flags
-- the sources are expected to be in src as described in <http://www.haskell.org/haskellwiki/structure_of_a_haskell_project>
-- the result of compilation (.o and .hi) are placed in the target/ directory
-- there is no main linkage by default to allow faster compilation feedback
defaultCompileFlags :: (DynFlags -> DynFlags)
defaultCompileFlags = \a-> a  {
    importPaths = ["src/","testsuite/tests/"]
    ,verbosity = 1
    ,outputFile = Just "target/Main"
    ,objectDir = Just "target"
    ,hiDir = Just "target"
    ,ghcLink = NoLink
  } 

-- | The default Listener configuration
-- Listened sources are all haskell sources in src/ and testsuite/
-- The default delay is 2 sec
defaultListenerInput :: ListenerInput                                   
defaultListenerInput = ListenerInput {
    fileListenInfo= FileListenInfo "." [] [".*\\.hs$"], 
    delay = 2000000
    }

-- | The default plugin map contains mapping for compile, help and exit action
defaultPluginMap :: PluginMap
defaultPluginMap = M.fromList list
  where list = [
                (Compile,runCompile),
                (Help,runHelp),
                (Quit,runExit)
              ]

defaultCommandMap :: CommandMap 
defaultCommandMap = M.fromList list
  where list = [
            ("Compile",Compile),
            ("Help", Help),
            ("QuickCheck",QuickCheck),
            ("q",Quit),
            ("Load",Load),
            ("Quit",Quit)
          ]

