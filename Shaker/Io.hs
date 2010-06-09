module Shaker.Io
 where

import Control.Monad
import System.Directory
import System.Time
import Data.List
import Shaker.Regex



listFiles :: FilePath -> [String] -> IO[FilePath]
listFiles dir ignore = 
    canonicalizePath dir >>= \curDir ->
    getDirectoryContents curDir >>= \res ->
    return $ filterListWithRegexp (convertToFullPath curDir res) ignore

convertToFullPath :: FilePath -> [FilePath] -> [FilePath]
convertToFullPath absDir lstFp = map (\a-> concat [absDir, "/",a]) lstFp


listModifiedFiles :: [(FilePath,ClockTime)] -> IO [(FilePath,ClockTime)]
listModifiedFiles tupleFpCl = filterM funFilFp tupleFpCl
  where funFilFp (fp, cl) = getModificationTime fp >>= \newCl -> 
          return $ ( newCl > cl )
                         
listCreatedFiles :: FilePath -> [String] -> [(FilePath,ClockTime)] -> IO[(FilePath,ClockTime)]
listCreatedFiles dir ignore tupleFpCl = 
    getCurrentFpCl dir ignore >>= \curFpCl ->
    return $ deleteFirstsBy (\c p -> fst c == fst p) curFpCl tupleFpCl
     

getCurrentFpCl :: FilePath -> [String] -> IO [(FilePath, ClockTime) ]
getCurrentFpCl dir ignore = listFiles dir ignore >>= \lstFp ->
      mapM getModificationTime lstFp >>= \lstCl ->
            zipWithM (\a b-> return (a,b)) lstFp lstCl
                  

