module Shaker.Io
 where

import Control.Monad
import System.Directory
import System.Time
import Data.List
import Shaker.Regex

listFiles :: FilePath -> [String] -> IO[FilePath]
listFiles dir ignore = getDirectoryContents dir >>= \res ->
    return $ filterListWithRegexp res ignore


listModifiedFiles :: [(FilePath,ClockTime)] -> IO [(FilePath,ClockTime)]
listModifiedFiles tupleFpCl = filterM funFilFp tupleFpCl
  where funFilFp (fp, cl) = getModificationTime fp >>=
                  \newCl -> return ( newCl /= cl )
                         
getCurrentFpCl :: FilePath -> [String] -> IO [(FilePath, ClockTime) ]
getCurrentFpCl dir ignore = listFiles dir ignore >>= \lstFp ->
      mapM getModificationTime lstFp >>= \lstCl ->
            zipWithM (\a b-> return (a,b)) lstFp lstCl
                  



