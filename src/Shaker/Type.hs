module Shaker.Type
 where

import System.Directory
import System.Time

-- |Agregate a FilePath with its modification time
data FileInfo = FileInfo FilePath ClockTime 
  deriving (Show,Eq)

hasSameFilePath :: FileInfo -> FileInfo -> Bool
hasSameFilePath (FileInfo fp1 _) (FileInfo fp2 _) = fp1 == fp2

-- | Represents directory to listen 
data FileListenInfo = FileListenInfo{
  dir :: FilePath     -- location of the listened directory
  ,ignore :: [String] -- ignore patterns
  }
  deriving (Show)


