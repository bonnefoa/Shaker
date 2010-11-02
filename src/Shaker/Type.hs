-- | Aggregate all types and data used through shaker
module Shaker.Type
 where

import Data.Monoid 
import Data.List

import Distribution.Simple.LocalBuildInfo

import DynFlags hiding (OneShot)
import qualified Data.Map as M
import Control.Monad.Reader
import System.Time(ClockTime)
import Control.Concurrent.MVar
import Control.Concurrent

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax

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
  shakerInputStateCommand :: InputCommand,
  shakerInputStateToken :: Token -- ^ Token is used to manage the token between action executor and command-line listener
}

-- | ShakerAction represents the differents actions realisable by shaker
data ShakerAction =
  Compile                            -- ^ Compile sources with ghc
    | FullCompile                    -- ^ Compile all hs sources with ghc
    | TestFramework                  -- ^ Execute both quickcheck and hunit using test framework
    | ModuleTestFramework            -- ^ Execute both quickcheck and hunit using test framework with module filtering
    | IntelligentTestFramework       -- ^ Execute both quickcheck and hunit using test framework on recompiled modules
    | IntelligentModuleTestFramework -- ^ Execute both quickcheck and hunit using test framework on recompiled modules
    | InvalidAction                  -- ^ Display an error when invalid action is inputed
    | Help                           -- ^ Display the help
    | Execute                        -- ^ Execute a command
    | Empty                          -- ^ Nothing to execute
    | Quit                           -- ^ Exit shaker
    | Clean                          -- ^ Delete generated
  deriving (Show,Eq,Ord)

-- | Command agregate a duration with an action
data Command = Command Duration [Action] 
  deriving (Show,Eq)

-- | Represents the global configuration of the system
data ShakerInput = ShakerInput {
  shakerCompileInputs :: [CompileInput]
  ,shakerListenerInput:: ListenerInput
  ,shakerPluginMap :: PluginMap
  ,shakerCommandMap :: CommandMap
  ,shakerArgument :: [String]
  ,shakerModifiedInfoFiles :: [FileInfo]
  ,shakerThreadData :: ThreadData 
  ,shakerInputState :: InputState 
  ,shakerLocalBuildInfo :: LocalBuildInfo
  ,shakerModuleData :: [ModuleData]
 }
 
data ThreadData = ThreadData {
    threadDataListenToken :: Token 
    ,threadDataQuitToken :: Token 
    ,threadDataListenList :: ThreadIdList
    ,threadDataQuitList :: ThreadIdList
 }
     
getListenThreadList :: ShakerInput -> ThreadIdList 
getListenThreadList = threadDataListenList . shakerThreadData
  
-- | Configuration flags to pass to the ghc compiler
data CompileInput = CompileInput{
  compileInputSourceDirs :: [String] -- ^ Source fileListenInfoDirectory of haskell files
  ,compileInputBuildDirectory :: String  -- ^ Destination of .o and .hi files
  ,compileInputDynFlags :: DynFlags->DynFlags -- ^ A transform fonction wich will takes the DynFlags of the current ghc session and change some values
  ,compileInputCommandLineFlags :: [String]  -- ^ The command line to pass options to pass to the ghc compiler
  ,compileInputTargetFiles :: [String] -- ^ List of files or list of modules to compile
}

-- | Default compilation shakerArgument.
-- Wall is activated by default
instance Monoid CompileInput where
  mempty = CompileInput {
    compileInputSourceDirs = ["."]
    ,compileInputBuildDirectory =  "dist/shakerTarget"  
    ,compileInputDynFlags = defaultCompileFlags  
    ,compileInputCommandLineFlags = ["-Wall"]
    ,compileInputTargetFiles = []
    }
  mappend cpIn1 cpIn2 = CompileInput {
    compileInputSourceDirs = nub $ compileInputSourceDirs cpIn1 `mappend` compileInputSourceDirs cpIn2
    ,compileInputBuildDirectory = compileInputBuildDirectory cpIn1
    ,compileInputDynFlags = compileInputDynFlags cpIn1 . compileInputDynFlags cpIn2
    ,compileInputCommandLineFlags = nub $ compileInputCommandLineFlags cpIn1 `mappend` compileInputCommandLineFlags cpIn2
    ,compileInputTargetFiles = nub $ compileInputTargetFiles cpIn1 `mappend` compileInputTargetFiles cpIn2
  }

instance Show CompileInput 
 where show (CompileInput src _ _ commandLine target) = 
         concat ["CompileInput |source : ",show src," |cmdLine : ",show commandLine," |targetfiles : ", show target]

-- | Configuration of the continuous listener
data ListenerInput = ListenerInput {
  listenerInputFiles :: [FileListenInfo] -- ^ The files to listen
  ,listenerInputDelay :: Int  -- ^ Delay beetween 2 check in microsecond
}

-- | The default Listener configuration
-- Listened sources are all haskell sources in .
-- The default listenerInputDelay is 2 sec
instance Monoid ListenerInput where 
  mempty = ListenerInput {
    listenerInputFiles = mempty
    ,listenerInputDelay = 1000000
    }
  mappend l1 l2 = ListenerInput {
    listenerInputFiles = listenerInputFiles l1 `mappend` listenerInputFiles l2
    ,listenerInputDelay = listenerInputDelay l1
    }

-- | Represents fileListenInfoDirectory to listen 
data FileListenInfo = FileListenInfo{
  fileListenInfoDir :: FilePath     -- ^ location of the listened fileListenInfoDirectory
  ,fileListenInfoIgnore :: [String] -- ^ fileListenInfoIgnore patterns
  ,fileListenInfoInclude :: [String] -- ^fileListenInfoInclude patterns
  }
  deriving (Show,Eq)

instance Monoid FileListenInfo where
  mempty = FileListenInfo "." defaultExclude defaultHaskellPatterns
  mappend f1 f2 = FileListenInfo { 
    fileListenInfoDir = fileListenInfoDir f1
    ,fileListenInfoIgnore = nub $ fileListenInfoIgnore f1 `mappend` fileListenInfoIgnore f2
    ,fileListenInfoInclude = nub $ fileListenInfoInclude f1 `mappend` fileListenInfoInclude f2
  }

-- |Agregate a FilePath with its modification time
data FileInfo = FileInfo {
  fileInfoFilePath :: FilePath
 ,fileInfoClockTime:: ClockTime 
  }
  deriving (Show,Eq)

data PackageData = PackageData {
    packageDataMapImportToModules :: MapImportToModules
    ,packageDataListProjectModules :: [String]
 }

data ModuleData = ModuleData {
  moduleDataModule :: Module
  ,moduleDataProperties :: [String]
  ,moduleDataAssertions :: [String]
  ,moduleDataTestCase :: [String]
 } deriving (Show)

type MapImportToModules = M.Map String [String]
-- | Represents the mapping beetween an action and the function to execute
type PluginMap = M.Map ShakerAction Plugin
-- | Represents the mapping between the command-line input and the action
type CommandMap = M.Map String ShakerAction 
-- | Represents an action of shaker
type Plugin = Shaker IO()

-- * Default data
 
-- | default dynamics flags
-- the sources are expected to be in src as described in <http://www.haskell.org/haskellwiki/structure_of_a_haskell_project>
-- the result of compilation (.o and .hi) are placed in the dist/shakerTarget
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

listTestLibs :: [String]
listTestLibs = ["QuickCheck","HUnit","test-framework-hunit","test-framework","test-framework-quickcheck2","shaker"] 

