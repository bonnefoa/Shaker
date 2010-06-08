module Shaker.Io
 where 

import System.Directory
import System.Time
import Data.List 
import Shaker.Regex

listFiles :: FilePath -> [String] -> IO[FilePath]
listFiles dir ignore = getDirectoryContents dir >>= \res ->
  return $ filterListWithRegexp res ignore

