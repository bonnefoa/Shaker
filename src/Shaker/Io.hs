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
listFiles :: FilePath -> [String] -> IO[FilePath]
listFiles dir ignore = 
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
listCreatedFiles :: FilePath -> [String] -> [FileInfo] -> IO[FileInfo]
listCreatedFiles dir ignore tupleFpCl = 
    getCurrentFpCl dir ignore >>= \curFpCl ->
    return $ deleteFirstsBy (hasSameFilePath) curFpCl tupleFpCl
     
-- |Get the list of FileInfo of the given directory
getCurrentFpCl :: FilePath -> [String] -> IO [FileInfo]
getCurrentFpCl dir ignore = listFiles dir ignore >>= \lstFp ->
      mapM getModificationTime lstFp >>= \lstCl ->
            zipWithM (\a b->return (FileInfo a b)) lstFp lstCl
                  



