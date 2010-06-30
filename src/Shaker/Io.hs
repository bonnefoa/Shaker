module Shaker.Io
 where

import Control.Monad
import System.Directory
import System.Time
import Data.List
import Shaker.Regex
import Shaker.Type

-- |Recursivly list all hs files
listProjectFiles :: IO([FilePath])
listProjectFiles = recurseListFiles $ FileListenInfo "." [] [".*\\.hs$"]

-- |Get the tuples of (newFiles,modifiedFiles) from given directory
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
listFiles fli@(FileListenInfo dir ignore include) = 
    canonicalizePath dir >>= \curDir ->
    getDirectoryContents curDir >>= \res ->
    return $ filteredList curDir res
    where filteredList curDir res = processListWithRegexp (convertToFullPath curDir res) ignore include

recurseListFiles :: FileListenInfo -> IO [FilePath]
recurseListFiles fli@(FileListenInfo dir ignore include) = 
  canonicalizePath dir >>= \curDir ->
  getDirectoryContents curDir >>= \content -> 
  filterM (doesDirectoryExist) (convertToFullPath curDir (removeDotDirectory content) ) >>= \directories ->
  mapM (\a -> recurseListFiles fli{dir=a}) directories >>= \sub ->  
  listFiles fli >>= \curListFiles ->
  return $ curListFiles ++ (concat $ sub)

convertToFullPath :: FilePath -> [FilePath] -> [FilePath]
convertToFullPath absDir lstFp = map (\a-> concat [absDir, "/",a]) lstFp

removeDotDirectory = filter (\a -> not $ isSuffixOf "." a )
