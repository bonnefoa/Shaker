module Shaker.Type
 where

import System.Directory
import System.Time
import Control.Monad.Reader
import Control.Concurrent.MVar (MVar)
import Control.Concurrent (ThreadId)
import Control.Monad.State
import DynFlags 
import qualified Data.Map as M

data Duration = OneShot | Continuous
  deriving (Show,Eq)
data Action = Load | Compile | QuickCheck |Help |Quit | Clean
  deriving (Show,Eq,Ord)
data Command = Command Duration Action
  deriving (Show,Eq)

type Input = MVar Command
type Token = MVar Int

data InputState = InputState {  
  input :: Input,
  token :: Token
}

data ShakerInput = ShakerInput {
  compileInput :: CompileInput,
  listenerInput :: ListenerInput,
  pluginMap :: PluginMap,
  commandMap :: CommandMap
}

getCompileInput (ShakerInput compileInput _ _ _ ) = compileInput
getListenerInput  (ShakerInput _ listenerInput _ _ )= listenerInput
getPluginMap (ShakerInput _ _ pluginMap _ )= pluginMap
getCommandMap (ShakerInput _ _ _ commandMap)= commandMap

data CompileInput = CompileInput{
  cfDynFlags :: (DynFlags->DynFlags)
}
-- ListenerStuff
data ListenerInput = ListenerInput {
  fileListenInfo :: FileListenInfo,
  delay :: Int  
}

-- | Represents directory to listen 
data FileListenInfo = FileListenInfo{
  dir :: FilePath     -- ^ location of the listened directory
  ,ignore :: [String] -- ^ ignore patterns
  ,include :: [String] -- ^include patterns
  }
  deriving (Show,Eq)
type CurrentFiles = MVar [FileInfo]
type ModifiedFiles = MVar [FileInfo]
type Job = MVar FileListenInfo

-- |Agregate a FilePath with its modification time
data FileInfo = FileInfo FilePath ClockTime 
  deriving (Show,Eq)
getFilePath (FileInfo fp _) = fp

data ListenState = ListenState {
  currentFiles :: CurrentFiles,
  modifiedFiles :: ModifiedFiles,
  threadIds :: [ThreadId]
}

getListenThreads (ListenState _ _ threads) = threads

type PluginMap = M.Map Action (ShakerInput -> IO())
type CommandMap = M.Map String Action 
type Plugin = ShakerInput -> IO()

