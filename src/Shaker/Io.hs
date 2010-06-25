module Shaker.Io
 where

import Control.Monad
import System.Directory
import System.Time
import Data.List
import Shaker.Regex
import Shaker.Type


listModifiedAndCreatedFiles :: FileListenInfo -> [FileInfo] -> IO([FileInfo],[FileInfo])
listModifiedAndCreatedFiles fileListen oldFileInfo = 
  getCurrentFpCl fileListen >>= \curFileInfo ->
  return $ (curFileInfo, curFileInfo \\ oldFileInfo)

-- |Get the list of FileInfo of the given directory
getCurrentFpCl :: FileListenInfo -> IO [FileInfo]
getCurrentFpCl fileListen = listFiles fileListen >>= \lstFp ->
      mapM getModificationTime lstFp >>= \lstCl ->
            zipWithM (\a b->return (FileInfo a b)) lstFp lstCl
                  
-- |List files in the given directory 
-- Files matching one regexp in the ignore argument are excluded
listFiles :: FileListenInfo -> IO[FilePath]
listFiles (FileListenInfo dir ignore) = 
    canonicalizePath dir >>= \curDir ->
    getDirectoryContents curDir >>= \res ->
    return $ filterListWithRegexp (convertToFullPath curDir res) ignore

convertToFullPath :: FilePath -> [FilePath] -> [FilePath]
convertToFullPath absDir lstFp = map (\a-> concat [absDir, "/",a]) lstFp

