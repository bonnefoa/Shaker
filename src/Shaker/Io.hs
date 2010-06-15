module Shaker.Io
 where

import Control.Monad
import System.Directory
import System.Time
import Data.List
import Shaker.Regex
import Shaker.Type

-- |List files in the given directory 
-- Files matching one regexp in the ignore argument are excluded
listFiles :: FileListenInfo -> IO[FilePath]
listFiles (FileListenInfo dir ignore) = 
    canonicalizePath dir >>= \curDir ->
    getDirectoryContents curDir >>= \res ->
    return $ filterListWithRegexp (convertToFullPath curDir res) ignore

convertToFullPath :: FilePath -> [FilePath] -> [FilePath]
convertToFullPath absDir lstFp = map (\a-> concat [absDir, "/",a]) lstFp

-- |List all files from the list which have been modified
listModifiedFiles :: [FileInfo] -> IO [FileInfo]
listModifiedFiles tupleFpCl = filterM funFilFp tupleFpCl
  where funFilFp (FileInfo fp cl) = getModificationTime fp >>= \newCl -> 
          return $ ( newCl > cl )

-- | List all files in the given directory which does not appear in the file list
listCreatedFiles :: FileListenInfo -> [FileInfo] -> IO[FileInfo]
listCreatedFiles fileListen tupleFpCl = 
    getCurrentFpCl fileListen >>= \curFpCl ->
    return $ deleteFirstsBy (hasSameFilePath) curFpCl tupleFpCl

listModifiedAndCreatedFiles :: FileListenInfo -> [FileInfo] -> IO[FileInfo]
listModifiedAndCreatedFiles fileListen [] = getCurrentFpCl fileListen
listModifiedAndCreatedFiles fileListen flInfo = 
   listModifiedFiles flInfo >>= \modifiedFiles ->
   listCreatedFiles fileListen flInfo >>= \createdFiles ->
   return (modifiedFiles ++ createdFiles) 

-- |Get the list of FileInfo of the given directory
getCurrentFpCl :: FileListenInfo -> IO [FileInfo]
getCurrentFpCl fileListen = listFiles fileListen >>= \lstFp ->
      mapM getModificationTime lstFp >>= \lstCl ->
            zipWithM (\a b->return (FileInfo a b)) lstFp lstCl
                  



