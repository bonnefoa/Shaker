module Shaker.Type
 where

import System.Time
import Control.Concurrent.MVar (MVar)
import Control.Concurrent (ThreadId)
import DynFlags 
import qualified Data.Map as M

-- | Duration define the life span of an action
data Duration = 
	OneShot     -- ^Execute the action and give back control
	| Continuous-- ^Execute the action when a source file modification is done until it is stopped
	deriving (Show,Eq)

-- | Action represents the differents actions realisable by shaker
data Action = 
	Load -- ^Load sources with hint
	| Compile -- ^ Compile sources with ghc
	| QuickCheck -- ^ Execute quickcheck properties
	| Help -- ^ Display the help
	| Quit -- ^ Exit shaker
	| Clean -- ^ Delete generated 
  deriving (Show,Eq,Ord)

-- | Command agregate a duration with an action
data Command = Command Duration Action
  deriving (Show,Eq)

-- | The input mvar is used to push the parsed command
type Input = MVar Command
-- | Token is used to manage the token between action executor and command-line listener
type Token = MVar Int

data InputState = InputState {  
  input :: Input,
  token :: Token
}

-- | Represents the global configuration of the system
data ShakerInput = ShakerInput {
  compileInput :: CompileInput 
  ,listenerInput :: ListenerInput
  ,pluginMap :: PluginMap
  ,commandMap :: CommandMap
}
  
-- | Configuration flags to pass to the ghc compiler
data CompileInput = CompileInput{
  cfDynFlags :: (DynFlags->DynFlags) -- ^ A transform fonction wich will takes the DynFlags of the current ghc session and change some values
  ,cfCommandLineFlags :: [String]  -- ^ The command line to pass options to pass to the ghc compiler
}

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

-- | MVar used to store currentFiles listed
type CurrentFiles = MVar [FileInfo]
-- | MVar used to store modifiedFiles since the last check
type ModifiedFiles = MVar [FileInfo]
-- | MVar used to pass action to the directory scanner
type Job = MVar [FileListenInfo]

-- |Agregate a FilePath with its modification time
data FileInfo = FileInfo FilePath ClockTime 
  deriving (Show,Eq)

-- | Agregate all information of listener
data ListenState = ListenState {
  currentFiles :: CurrentFiles  -- ^ Files found in the last check
  ,modifiedFiles :: ModifiedFiles -- ^ Differences between last and before last check
  ,threadIds :: [ThreadId] -- ^ List of all forks id initialized
}

-- | Represents the mapping beetween an action and the function to execute
type PluginMap = M.Map Action Plugin
-- | Represents the mapping between the command-line input and the action
type CommandMap = M.Map String Action 
-- | Represents an action of shaker
type Plugin = ShakerInput -> IO()




data CabalInfo = CabalInfo {
    sourceDir :: [String] -- ^ Location of hs sources
    ,modules :: [String] -- ^ Exposed modules or main executable. It will be the target of the compilation.
    ,compileOption :: [String] -- ^ Options to pass to the compiler
--    ,packageType :: PackageType -- ^ Type of cabal information (Library or Executable)
    ,packagesToExpose :: [String] -- ^ List of package to expose 
  }
 deriving (Show)

data PackageType = ExecutableType | LibraryType
 deriving (Show)
 
