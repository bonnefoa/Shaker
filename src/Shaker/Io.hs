-- | Manage all file operations like listing files with includes and exclude patterns
-- and file filtering
module Shaker.Io(
  -- * List files functions
  listModifiedAndCreatedFiles
  ,listFiles
  ,getCurrentFpCl
  ,recurseMultipleListFiles
  ,recurseListFiles 
  -- * Test file property
  ,isFileContainingMain
  ,isFileContainingTH
  -- * Default patterns
  ,defaultHaskellPatterns
  ,defaultExclude
)
 where
 
import qualified Data.ByteString.Char8 as L 
import Control.Monad
import System.Directory
import Data.List
import Shaker.Regex
import Shaker.Type

-- |Get the tuples of (newFiles,modifiedFiles) from given list of directory
listModifiedAndCreatedFiles :: [FileListenInfo] -> [FileInfo] -> IO ([FileInfo],[FileInfo])
listModifiedAndCreatedFiles job curFiles = do
   lstNewAndModifier <- mapM (`listModifiedAndCreatedFiles'` curFiles) job
   return $ foldl1 (\(a,b) (c,d) -> (a++c,b++d)) lstNewAndModifier
   
-- |Get the tuples of (newFiles,modifiedFiles) from given directory
listModifiedAndCreatedFiles' :: FileListenInfo -> [FileInfo] -> IO([FileInfo],[FileInfo])
listModifiedAndCreatedFiles' fileListen oldFileInfo = do
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

recurseMultipleListFiles :: [FileListenInfo] -> IO [FilePath]
recurseMultipleListFiles flis = liftM concat $ mapM recurseListFiles flis

-- | Recursively list all files
-- All non matching files are excluded
recurseListFiles :: FileListenInfo -> IO [FilePath]
recurseListFiles fli@(FileListenInfo inputDir _ _) = do
  curDir <- canonicalizePath inputDir
  content <- getDirectoryContents curDir
  directories <- filterM doesDirectoryExist (convertToFullPath curDir (removeDotDirectory content) ) 
  sub <- mapM (\a -> recurseListFiles fli{dir=a}) directories
  curListFiles <-  listFiles fli
  return $ curListFiles ++ concat sub

isFileContainingTH :: FilePath -> IO Bool
isFileContainingTH fp = isFileContaining fp (L.pack "$(" `L.isInfixOf`)

isFileContainingMain :: FilePath -> IO Bool
isFileContainingMain fp = isFileContaining fp (\a -> L.pack "main " `L.isPrefixOf` a || L.pack "main:" `L.isPrefixOf` a)

isFileContaining :: FilePath -> (L.ByteString -> Bool) -> IO Bool
isFileContaining fp pat = do
   byStr <- L.readFile fp
   return $ any pat $ L.lines byStr

convertToFullPath :: FilePath -> [FilePath] -> [FilePath]
convertToFullPath absDir = map (\a-> concat [absDir, "/",a]) 

removeDotDirectory :: [String] -> [String]
removeDotDirectory = filter (not . isSuffixOf "."  ) 

