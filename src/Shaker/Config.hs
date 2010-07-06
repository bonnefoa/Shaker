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
  compileInput = defaultCompileInput,
  listenerInput = defaultListenerInput,
  pluginMap = defaultPluginMap,
  commandMap = defaultCommandMap
  }

defaultCompileInput :: CompileInput
defaultCompileInput = CompileInput defaultCompileFlags

defaultCompileFlags :: (DynFlags -> DynFlags)
defaultCompileFlags = \a-> a  {
    importPaths = ["src/","testsuite/tests/"], 
    verbosity = 1, 
    outputFile = Just "target/Main",
    objectDir = Just "target",
    hiDir = Just "target",
    ghcLink = NoLink,
    flags =standardWarnings 
  } 

standardWarnings :: [DynFlag]
standardWarnings
    = [ Opt_WarnWarningsDeprecations,
        Opt_WarnDeprecatedFlags,
        Opt_WarnUnrecognisedPragmas,
        Opt_WarnOverlappingPatterns,
        Opt_WarnMissingFields,
        Opt_WarnMissingMethods,
        Opt_WarnDuplicateExports,
        Opt_WarnLazyUnliftedBindings,
        Opt_WarnDodgyForeignImports,
        Opt_WarnWrongDoBind
      ]

defaultListenerInput :: ListenerInput                                   
defaultListenerInput = ListenerInput {
    fileListenInfo= FileListenInfo "." [] [".*\\.hs$"], 
    delay = 2*10^6
    }

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
