-- | Aggregate all types and data used through shaker
module Shaker.Type
 where

import DynFlags
import qualified Data.Map as M
import Control.Monad.Reader
import System.Time(ClockTime)

-- | Environnement containing the project configuration.
-- It is generated at startup and won't change
type Shaker  = ReaderT ShakerInput 
-- | Environnement for the project compilation
-- This environnement can change depending on the compile 
-- action called
type CompileM = Reader CompileInput

-- | Duration define the life span of an action
data Duration = 
	OneShot     -- ^Execute the action and give back control
	| Continuous-- ^Execute the action when a source file modification is done until it is stopped
	deriving (Show,Eq)

-- | Action represents the differents action with arguments
data Action = 
  Action ShakerAction
  | ActionWithArg ShakerAction String
  deriving (Show,Eq,Ord)

-- | ShakerAction represents the differents actions realisable by shaker
data ShakerAction = 
	Compile -- ^ Compile sources with ghc
	| FullCompile -- ^ Compile all hs sources with ghc
	| QuickCheck -- ^ Execute quickcheck properties
	| HUnit -- ^ Execute hunit tests
	| Help -- ^ Display the help
        | Execute -- ^ Execute a command
	| Quit -- ^ Exit shaker
	| Clean -- ^ Delete generated 
  deriving (Show,Eq,Ord)

-- | Command agregate a duration with an action
data Command = Command Duration [Action] 
  deriving (Show,Eq)

-- | Represents the global configuration of the system
data ShakerInput = ShakerInput {
  compileInputs :: [CompileInput]
  ,listenerInput :: ListenerInput
  ,pluginMap :: PluginMap
  ,commandMap :: CommandMap
  ,argument :: Maybe String
  ,modifiedFiled :: [FilePath]
}
  
-- | Configuration flags to pass to the ghc compiler
data CompileInput = CompileInput{
  cfSourceDirs :: [String] -- ^ Source directory of haskell files
  ,cfDescription :: String -- ^ Desctipition of the compile input (executable or library if comming from cabal)
  ,cfCompileTarget :: String  -- ^ Destination of .o and .hi files
  ,cfDynFlags :: (DynFlags->DynFlags) -- ^ A transform fonction wich will takes the DynFlags of the current ghc session and change some values
  ,cfCommandLineFlags :: [String]  -- ^ The command line to pass options to pass to the ghc compiler
  ,cfTargetFiles :: [String] -- ^ List of files or list of modules to compile
}

instance Show CompileInput 
 where show (CompileInput src desc _ _ commandLine target) = 
         concat ["CompileInput |source : ",show src," |desc : ",desc," |cmdLine : ",show commandLine," |targetfiles : ", show target]

-- | Configuration of the continuous listener
data ListenerInput = ListenerInput {
  fileListenInfo :: [FileListenInfo] -- ^ The files to listen
  ,delay :: Int  -- ^ Delay beetween 2 check in microsecond
}

-- | Represents directory to listen 
data FileListenInfo = FileListenInfo{
  dir :: FilePath     -- ^ location of the listened directory
  ,ignore :: [String] -- ^ ignore patterns
  ,include :: [String] -- ^include patterns
  }
  deriving (Show,Eq)

-- |Agregate a FilePath with its modification time
data FileInfo = FileInfo {
  fileInfoFilePath :: FilePath
 ,fileInfoClockTime:: ClockTime 
  }
  deriving (Show,Eq)
  
-- | Represents the mapping beetween an action and the function to execute
type PluginMap = M.Map ShakerAction Plugin
-- | Represents the mapping between the command-line input and the action
type CommandMap = M.Map String ShakerAction 
-- | Represents an action of shaker
type Plugin = Shaker IO()


-- * Default data

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

-- | Default haskell file pattern : *.hs
defaultHaskellPatterns :: [String]
defaultHaskellPatterns = [".*\\.hs$"]

-- | Default exclude pattern : Setup.hs
defaultExclude :: [String]
defaultExclude =  [".*Setup\\.hs$"]
