module Shaker.Type
 where

import DynFlags(DynFlags)
import Shaker.Io(FileListenInfo)
import qualified Data.Map as M
import Control.Monad.Reader

type Shaker  = ReaderT ShakerInput 
type CompileM = Reader CompileInput

-- | Duration define the life span of an action
data Duration = 
	OneShot     -- ^Execute the action and give back control
	| Continuous-- ^Execute the action when a source file modification is done until it is stopped
	deriving (Show,Eq)

-- | Action represents the differents actions realisable by shaker
data Action = 
	Compile -- ^ Compile sources with ghc
	| FullCompile -- ^ Compile all hs sources with ghc
	| QuickCheck -- ^ Execute quickcheck properties
	| Help -- ^ Display the help
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
  
-- | Represents the mapping beetween an action and the function to execute
type PluginMap = M.Map Action Plugin
-- | Represents the mapping between the command-line input and the action
type CommandMap = M.Map String Action 
-- | Represents an action of shaker
type Plugin = Shaker IO()

