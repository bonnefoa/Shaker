module Shaker.Io
 where
 
import Control.Monad
import System.Directory
import Data.List
import Shaker.Regex
import Shaker.Type

-- |Get the tuples of (newFiles,modifiedFiles) from given directory
listModifiedAndCreatedFiles :: FileListenInfo -> [FileInfo] -> IO([FileInfo],[FileInfo])
listModifiedAndCreatedFiles fileListen oldFileInfo = do
  curFileInfo <- getCurrentFpCl fileListen
  return (curFileInfo, curFileInfo \\ oldFileInfo)

-- |Get the list of FileInfo of the given directory
getCurrentFpCl :: FileListenInfo -> IO [FileInfo]
getCurrentFpCl fileListen = do 
      lstFp <- recurseListFiles fileListen 
      lstCl <- mapM getModificationTime lstFp 
      zipWithM (\a b->return (FileInfo a b)) lstFp lstCl
                  
-- |List files in the given directory 
-- Files matching one regexp in the ignore argument are excluded
listFiles :: FileListenInfo -> IO[FilePath]
listFiles (FileListenInfo inputDir inputIgnore inputInclude) = do
    curDir <- canonicalizePath inputDir 
    res <- getDirectoryContents curDir
    return $ filteredList curDir res
    where filteredList curDir res = processListWithRegexp (convertToFullPath curDir res) inputIgnore inputInclude

-- | Recursively list all files
recurseListFiles :: FileListenInfo -> IO [FilePath]
recurseListFiles fli@(FileListenInfo inputDir _ _) = do
  curDir <- canonicalizePath inputDir
  content <- getDirectoryContents curDir
  directories <- filterM doesDirectoryExist (convertToFullPath curDir (removeDotDirectory content) ) 
  sub <- mapM (\a -> recurseListFiles fli{dir=a}) directories
  curListFiles <-  listFiles fli
  return $ curListFiles ++ concat sub

convertToFullPath :: FilePath -> [FilePath] -> [FilePath]
convertToFullPath absDir = map (\a-> concat [absDir, "/",a]) 

removeDotDirectory :: [String] -> [String]
removeDotDirectory = filter (\a -> not $ isSuffixOf "." a ) 
