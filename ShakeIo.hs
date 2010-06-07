module ShakeIo
 where 

import System.Directory
import Data.List 

listFiles :: FilePath -> [String] -> IO[FilePath]
listFiles dir ignore = getDirectoryContents dir >>= \res ->
  return $ foldl (\prec ig -> delete ig prec) res ignore

