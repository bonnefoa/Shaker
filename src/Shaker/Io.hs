module Shaker.Io(
  listModifiedAndCreatedFiles
  ,getCurrentFpCl
  ,recurseMultipleListFiles
  ,listFiles
  ,recurseListFiles 
  ,FileInfo(FileInfo)
  ,FileListenInfo(..)
)
 where
 
import Control.Monad
import System.Directory
import Data.List
import Shaker.Regex
import System.Time

-- | Represents directory to listen 
data FileListenInfo = FileListenInfo{
  dir :: FilePath     -- ^ location of the listened directory
  ,ignore :: [String] -- ^ ignore patterns
  ,include :: [String] -- ^include patterns
  }
  deriving (Show,Eq)

-- |Agregate a FilePath with its modification time
data FileInfo = FileInfo FilePath ClockTime 
  deriving (Show,Eq)

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
