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

hasSameFilePath :: FileInfo -> FileInfo -> Bool
hasSameFilePath (FileInfo fp1 _) (FileInfo fp2 _) = fp1 == fp2

-- | Represents directory to listen 
data FileListenInfo = FileListenInfo{
  dir :: FilePath     -- ^ location of the listened directory
  ,ignore :: [String] -- ^ ignore patterns
  ,include :: [String] -- ^include patterns
  }
  deriving (Show,Eq)

data ListenState = ListenState CurrentFiles ModifiedFiles ThreadId ThreadId

type Input = MVar String
type Token = MVar Int
data InputState = InputState Input Token ThreadId 

data ShakerState = ShakerState {
  listenState :: ListenState -- ^ Listen state information (var and thread ids) 
  ,inputState :: InputState -- ^ Input information 
}

-- | The shaker monad 
type Shaker = StateT ShakerState IO

