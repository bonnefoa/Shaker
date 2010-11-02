-- | Utilities function for compilation and haskell file management
module Shaker.SourceHelper(
  -- * Compile input management
  CompileFile(..)
  ,constructCompileFileList
  ,getFullCompileCompileInput
  ,getFullCompileCompileInputNonMain
  -- * Target files filtering
  ,setAllHsFilesAsTargets
  ,removeFileWithMain
  ,fillCompileInputWithStandardTarget
)
 where

import Data.List
import Data.Monoid
import Shaker.Io
import Shaker.Type

import Control.Monad.Reader(ask, asks, lift, runReader, Reader)
import Control.Arrow

import System.FilePath
import Language.Haskell.Syntax

type CompileR = Reader [CompileFile]

data CompileFile = CompileFile {
  compileFileFilePath :: FilePath 
  ,compileFileHasMain :: Bool 
 } deriving Show

-- | Build the list of haskell source files located in 
-- CompileInput source fileListenInfoDirs
constructCompileFileList :: Shaker IO [CompileFile] 
constructCompileFileList = do
  fli <- asks (shakerListenerInput >>> listenerInputFiles) 
  files <- lift $ fmap nub (recurseMultipleListFiles fli)
  lift $ mapM constructCompileFile files
  
-- | Build an individual CompileFile. 
constructCompileFile :: FilePath -> IO CompileFile      
constructCompileFile fp = do
  hasMain <- isFileContainingMain fp
  return $ CompileFile fp hasMain 

-- | Configure the CompileInput with all haskell files configured as targets
setAllHsFilesAsTargets :: CompileInput -> CompileR CompileInput
setAllHsFilesAsTargets cpIn = do
  files <- ask
  return cpIn {compileInputTargetFiles = map compileFileFilePath files }

removeFileWithMain :: CompileInput -> CompileR CompileInput
removeFileWithMain = removeFileWithPredicate compileFileHasMain

removeFileWithPredicate :: (CompileFile -> Bool) -> CompileInput -> CompileR CompileInput
removeFileWithPredicate predicate cpIn = do 
  cpFl <- ask 
  let toRemove = map compileFileFilePath $ filter predicate cpFl
  return $ cpIn {compileInputTargetFiles =  targets \\ toRemove}
  where targets = compileInputTargetFiles cpIn

-- | Fill compile input with every haskell files in the project except those
-- containing main and template haskell
fillCompileInputWithStandardTarget :: CompileInput -> CompileR CompileInput 
fillCompileInputWithStandardTarget cpIn = setAllHsFilesAsTargets cpIn >>= removeFileWithMain 

getFullCompileCompileInput :: Shaker IO [CompileInput]
getFullCompileCompileInput = do
  cpIn <- fmap mconcat  (asks shakerCompileInputs )
  cfFlList <- constructCompileFileList 
  let (mainFiles, nonMainFiles) = partition compileFileHasMain cfFlList
  let libraries = runReader (setAllHsFilesAsTargets cpIn) nonMainFiles
  let executables = map (runReader (setAllHsFilesAsTargets cpIn) ) $ split mainFiles 
  return $ libraries : executables
  where split = groupBy (\_ _ -> False) 

getFullCompileCompileInputNonMain :: Shaker IO CompileInput
getFullCompileCompileInputNonMain = do
  cpIn <- fmap mconcat  (asks shakerCompileInputs )
  cfFlList <- constructCompileFileList 
  let (_, nonMainFiles) = partition compileFileHasMain cfFlList
  let libraries = runReader (setAllHsFilesAsTargets cpIn) nonMainFiles
  return libraries 
