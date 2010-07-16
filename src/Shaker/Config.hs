-- | Contains the default configuration of shaker
module Shaker.Config
 where

import Shaker.Type
import Shaker.Io(FileListenInfo(..))
import Shaker.Cli(InputState(..))
import DynFlags
import qualified Data.Map as M (fromList)
import Shaker.Action.Compile
import Shaker.Action.Clean
import Shaker.Action.Standard
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

defaultHaskellPatterns :: [String]
defaultHaskellPatterns = [".*\\.hs$"]

-- | The default plugin map contains mapping for compile, help and exit action 
defaultPluginMap :: PluginMap
defaultPluginMap = M.fromList $ map (\(a,b) -> (a, runStartAction >> b >> runEndAction)) list
  where list = [
                (Compile,runCompile ),
                (Help,runHelp),
                (Clean,runClean),
                (Quit,runExit)
              ]

defaultCommandMap :: CommandMap 
defaultCommandMap = M.fromList list
  where list = [
            ("Compile",Compile),
            ("Help", Help),
            ("QuickCheck",QuickCheck),
            ("Clean",Clean),
            ("q",Quit),
            ("Load",Load),
            ("Quit",Quit)
          ]

defaultInputState :: IO InputState
defaultInputState = do
  inputMv <- newEmptyMVar 
  tokenMv <- newEmptyMVar  
  return InputState { input = inputMv, token =  tokenMv } 

