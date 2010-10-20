-- | Aggregate all types and data used through shaker
module Shaker.Type
 where

import Data.Monoid 
import Data.List

import DynFlags hiding (OneShot)
import qualified Data.Map as M
import Control.Monad.Reader
import System.Time(ClockTime)
import Control.Concurrent.MVar
import Control.Concurrent

-- | Environnement containing the project configuration.
-- It is generated at startup and won't change
type Shaker  = ReaderT ShakerInput 
type ShakerR  = Reader ShakerInput 

type ThreadIdList = MVar [ThreadId]
type Token = MVar Int

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
  | ActionWithArg ShakerAction [String]
  deriving (Show,Eq,Ord)
 
-- | The input mvar is used to push the parsed command
type InputCommand = MVar (Maybe Command)

data InputState = InputState {  
  inputStateCommand :: InputCommand,
  inputStateToken :: Token -- ^ Token is used to manage the token between action executor and command-line listener
}

-- | ShakerAction represents the differents actions realisable by shaker
data ShakerAction = 
	Compile -- ^ Compile sources with ghc
	| FullCompile -- ^ Compile all hs sources with ghc
        | TestFramework -- ^ Execute both quickcheck and hunit using test framework
        | ModuleTestFramework -- ^ Execute both quickcheck and hunit using test framework with module filtering
        | IntelligentTestFramework -- ^ Execute both quickcheck and hunit using test framework on recompiled modules
        | IntelligentModuleTestFramework -- ^ Execute both quickcheck and hunit using test framework on recompiled modules
        | InvalidAction -- ^ Display an error when invalid action is inputed
	| Help -- ^ Display the help
        | Execute -- ^ Execute a command
        | Empty -- ^ Nothing to execute 
	| Quit -- ^ Exit shaker
	| Clean -- ^ Delete generated 
  deriving (Show,Eq,Ord)

-- | Command agregate a duration with an action
data Command = Command Duration [Action] 
  deriving (Show,Eq)

-- | Represents the global configuration of the system
data ShakerInput = ShakerInput {
  shakerCompileInputs :: [CompileInput]
  ,listenerInput :: ListenerInput
  ,pluginMap :: PluginMap
  ,commandMap :: CommandMap
  ,argument :: [String]
  ,modifiedInfoFiles :: [FileInfo]
  ,threadData :: ThreadData 
  ,inputState :: InputState 
 }  
 
data ThreadData = ThreadData {
    listenToken :: Token 
    ,quitToken :: Token 
    ,threadIdListenList :: ThreadIdList
    ,threadIdQuitList :: ThreadIdList
 }
     
getListenThreadList :: ShakerInput -> ThreadIdList 
getListenThreadList = threadIdListenList . threadData
  
-- | Configuration flags to pass to the ghc compiler
data CompileInput = CompileInput{
  cfSourceDirs :: [String] -- ^ Source directory of haskell files
  ,cfDescription :: String -- ^ Desctipition of the compile input (executable or library if comming from cabal)
  ,cfCompileTarget :: String  -- ^ Destination of .o and .hi files
  ,cfDynFlags :: DynFlags->DynFlags -- ^ A transform fonction wich will takes the DynFlags of the current ghc session and change some values
  ,cfCommandLineFlags :: [String]  -- ^ The command line to pass options to pass to the ghc compiler
  ,cfTargetFiles :: [String] -- ^ List of files or list of modules to compile
}

-- | Default compilation argument.
-- Wall is activated by default
instance Monoid CompileInput where
  mempty = CompileInput {
    cfSourceDirs = ["."]
    ,cfDescription = "Default compile inputs"
    ,cfCompileTarget =  "dist/shakerTarget"  
    ,cfDynFlags = defaultCompileFlags  
    ,cfCommandLineFlags = ["-Wall"]
    ,cfTargetFiles = []
    }
  mappend cpIn1 cpIn2 = CompileInput {
    cfSourceDirs = nub $ cfSourceDirs cpIn1 `mappend` cfSourceDirs cpIn2
    ,cfDescription = "Merge : " ++ cfDescription cpIn1 ++ cfDescription cpIn2
    ,cfCompileTarget = cfCompileTarget cpIn1
    ,cfDynFlags = cfDynFlags cpIn1 . cfDynFlags cpIn2
    ,cfCommandLineFlags = nub $ cfCommandLineFlags cpIn1 `mappend` cfCommandLineFlags cpIn2
    ,cfTargetFiles = nub $ cfTargetFiles cpIn1 `mappend` cfTargetFiles cpIn2
  }

instance Show CompileInput 
 where show (CompileInput src desc _ _ commandLine target) = 
         concat ["CompileInput |source : ",show src," |desc : ",desc," |cmdLine : ",show commandLine," |targetfiles : ", show target]

-- | Configuration of the continuous listener
data ListenerInput = ListenerInput {
  fileListenInfo :: [FileListenInfo] -- ^ The files to listen
  ,delay :: Int  -- ^ Delay beetween 2 check in microsecond
}

-- | The default Listener configuration
-- Listened sources are all haskell sources in .
-- The default delay is 2 sec
instance Monoid ListenerInput where 
  mempty = ListenerInput {
    fileListenInfo = mempty
    ,delay = 2000000
    }
  mappend l1 l2 = ListenerInput {
    fileListenInfo = fileListenInfo l1 `mappend` fileListenInfo l2
    ,delay = delay l1
    }

-- | Represents directory to listen 
data FileListenInfo = FileListenInfo{
  dir :: FilePath     -- ^ location of the listened directory
  ,ignore :: [String] -- ^ ignore patterns
  ,include :: [String] -- ^include patterns
  }
  deriving (Show,Eq)

instance Monoid FileListenInfo where
  mempty = FileListenInfo "." defaultExclude defaultHaskellPatterns
  mappend f1 f2 = FileListenInfo { 
    dir = dir f1
    ,ignore = nub $ ignore f1 `mappend` ignore f2
    ,include = nub $ include f1 `mappend` include f2
  }

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


-- | default dynamics flags
-- the sources are expected to be in src as described in <http://www.haskell.org/haskellwiki/structure_of_a_haskell_project>
-- the result of compilation (.o and .hi) are placed in the target/ directory
-- there is no main linkage by default to allow faster compilation feedback
defaultCompileFlags :: (DynFlags -> DynFlags)
defaultCompileFlags a = a  {
    verbosity = 1
    ,ghcLink = NoLink
} 

-- | Default haskell file pattern : *.hs
defaultHaskellPatterns :: [String]
defaultHaskellPatterns = [".*\\.hs$", ".*\\.lhs"]

-- | Default exclude pattern : Setup.hs
defaultExclude :: [String]
defaultExclude =  [".*Setup\\.lhs$",".*Setup\\.hs$", ".*/\\."]

exitCommand :: Command
exitCommand = Command OneShot [Action Quit]

emptyCommand :: Command 
emptyCommand = Command OneShot [Action Empty] 

