-- | Manage all file operations like listing files with includes and exclude patterns
-- and file filtering
module Shaker.Io(
  -- * List files functions
  listModifiedAndCreatedFiles
  ,listFiles
  ,getCurrentFpCl
  ,recurseMultipleListFiles
  ,recurseListFiles
  ,mapImportToModules
  -- * Test file property
  ,isFileContainingMain
  ,isFileContainingTH
  -- * Default patterns
  ,defaultHaskellPatterns
  ,defaultExclude
  -- * Exception handling
  ,handleActionInterrupt
  ,handleIOException
)
 where
 
import Control.Monad
import Control.Arrow
import System.Directory
import qualified Data.Map as M
import Data.List
import Data.Maybe

import Shaker.Regex
import Shaker.Type

import Language.Haskell.Parser
import Language.Haskell.Syntax

import qualified Control.Exception as C
import qualified Data.ByteString.Char8 as L 

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
isFileContainingMain fp = isFileContaining fp $ (\a -> L.pack "main " `L.isPrefixOf` a || L.pack "main:" `L.isPrefixOf` a) . L.dropWhile (== ' ')

isFileContaining :: FilePath -> (L.ByteString -> Bool) -> IO Bool
isFileContaining fp pat = do
   byStr <- L.readFile fp
   return $ any pat $ L.lines byStr

convertToFullPath :: FilePath -> [FilePath] -> [FilePath]
convertToFullPath absDir = map (\a-> concat [absDir, "/",a]) 

removeDotDirectory :: [String] -> [String]
removeDotDirectory = filter (not . isSuffixOf "."  ) 

mapImportToModules :: IO ( M.Map String [String] )
mapImportToModules = do
   files <- recurseListFiles (FileListenInfo "." defaultExclude defaultHaskellPatterns)
   fileContentList <- mapM readFile files
   return $ constructImportToModules $ nub $ map getImport (mapMaybe parseHs fileContentList)
   where getImport :: HsModule -> (String, [String]) 
         getImport (HsModule _ moduleName _ listImportDecl _) = (unModule moduleName, map (unModule . importModule) listImportDecl)
         parseHs content = case parseModule content of
                                ParseOk val -> Just val
                                _ -> Nothing
         unModule (Module v) = v

constructImportToModules :: [ ( String, [String] ) ] -> M.Map String [String]
constructImportToModules moduleToImports = M.fromList listKeysWithModules
  where listProjectModules = map fst moduleToImports
        listKeys = (nub $ concatMap snd moduleToImports) \\ listProjectModules
        listKeysWithModules = map ( \ imp -> (imp, getAllModulesForImport imp) ) listKeys
        getAllModulesForImport imp = filter ( \ (_, lstImp) ->  imp `elem` lstImp ) >>> map fst $ moduleToImports 

-- * Exception management

handleActionInterrupt :: IO() -> IO()
handleActionInterrupt =  C.handle catchAll
  where catchAll :: C.SomeException -> IO ()
        catchAll e = putStrLn ("Shaker caught " ++ show e ) >>  return ()

handleIOException :: IO() -> IO()
handleIOException = C.handle catchIO
  where catchIO :: C.IOException -> IO()
        catchIO e = putStrLn ("Shaker caught " ++ show e ) >>  return ()



