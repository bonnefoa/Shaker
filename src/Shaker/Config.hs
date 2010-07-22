-- | Contains the default configuration of shaker
module Shaker.Config
 where

import Shaker.Type
import Shaker.PluginConfig
import Shaker.Io(FileListenInfo(..),defaultHaskellPatterns)
import Shaker.Cli(InputState(..))
import DynFlags
import Control.Concurrent


defaultInput ::ShakerInput  
defaultInput = ShakerInput {
  compileInputs = [defaultCompileInput],
  listenerInput = defaultListenerInput,
  pluginMap = defaultPluginMap,
  commandMap = defaultCommandMap
  }

-- | Default compilation argument.
-- Wall is activated by default
defaultCompileInput :: CompileInput
defaultCompileInput = CompileInput {
  cfSourceDirs= ["src/","testsuite/tests/"]
  ,cfDescription = "Default Compilation"
  ,cfCompileTarget =  "target"  
  ,cfDynFlags = defaultCompileFlags  
  ,cfCommandLineFlags = ["-Wall"]
  ,cfTargetFiles = []
}

-- | default dynamics flags
-- the sources are expected to be in src as described in <http://www.haskell.org/haskellwiki/structure_of_a_haskell_project>
-- the result of compilation (.o and .hi) are placed in the target/ directory
-- there is no main linkage by default to allow faster compilation feedback
defaultCompileFlags :: (DynFlags -> DynFlags)
defaultCompileFlags = \a-> a  {
    verbosity = 1
    ,ghcLink = NoLink
} 

-- | The default Listener configuration
-- Listened sources are all haskell sources in src/ and testsuite/
-- The default delay is 2 sec
defaultListenerInput :: ListenerInput                                   
defaultListenerInput = ListenerInput {
    fileListenInfo= [FileListenInfo "src/" [] defaultHaskellPatterns, FileListenInfo "testsuite/" [] defaultHaskellPatterns ]
    ,delay = 2000000
    }

defaultInputState :: IO InputState
defaultInputState = do
  inputMv <- newEmptyMVar 
  tokenMv <- newEmptyMVar  
  return InputState { input = inputMv, token =  tokenMv } 

