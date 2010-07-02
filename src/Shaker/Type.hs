module Shaker.Type
 where

import System.Directory
import System.Time
import Control.Concurrent.MVar (MVar)
import Control.Concurrent (ThreadId)
import Control.Monad.State

data Duration = OneShot | Continuous
  deriving (Show,Eq)
data Action = Load | Compile | QuickCheck |Help |Quit
  deriving (Show,Eq)
data Command = Command Duration Action
  deriving (Show,Eq)

type CurrentFiles = MVar [FileInfo]
type ModifiedFiles = MVar [FileInfo]
type Job = MVar FileListenInfo
-- |Agregate a FilePath with its modification time
data FileInfo = FileInfo FilePath ClockTime 
  deriving (Show,Eq)

getFilePath (FileInfo fp _) = fp

hasSameFilePath :: FileInfo -> FileInfo -> Bool
hasSameFilePath (FileInfo fp1 _) (FileInfo fp2 _) = fp1 == fp2

-- | Represents directory to listen 
data FileListenInfo = FileListenInfo{
  dir :: FilePath     -- ^ location of the listened directory
  ,ignore :: [String] -- ^ ignore patterns
  ,include :: [String] -- ^include patterns
  }
  deriving (Show,Eq)

type Input = MVar Command
type Token = MVar Int
data ListenState = ListenState {
  currentFiles :: CurrentFiles,
  modifiedFiles :: ModifiedFiles,
  threadListen :: ThreadId,
  threadSchedule :: ThreadId
}

data InputState = InputState {  
  input :: Input,
  token :: Token,
  threadCli :: ThreadId
}

-- | The shaker monad 
type InputShaker = StateT InputState 

  
