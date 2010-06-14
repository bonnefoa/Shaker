module Shaker.Type
 where

import System.Directory
import System.Time

-- |Agregate a FilePath with its modification time
data FileInfo = FileInfo FilePath ClockTime
  deriving (Show)

hasSameFilePath :: FileInfo -> FileInfo -> Bool
hasSameFilePath (FileInfo fp1 _) (FileInfo fp2 _) = fp1 == fp2

