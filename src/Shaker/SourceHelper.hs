-- | Utilities function for compilation and haskell file management
module Shaker.SourceHelper(
  -- * Compile input management
  CompileFile(..)
  ,mergeCompileInputsSources
  ,constructCompileFileList
  ,getFullCompileCompileInput
  ,getFullCompileCompileInputNonMain
  -- * Target files filtering
  ,setAllHsFilesAsTargets
  ,removeFileWithMain
  ,removeFileWithTemplateHaskell
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

type CompileR = Reader [CompileFile]

data CompileFile = CompileFile {
  cfFp :: FilePath 
  ,cfHasMain :: Bool 
  ,cfHasTH :: Bool
 } deriving Show

-- | Build the list of haskell source files located in 
-- CompileInput source dirs
constructCompileFileList :: Shaker IO [CompileFile] 
constructCompileFileList = do
  fli <- asks (shakerListenerInput>>> fileListenInfo) 
  files <- lift $ fmap nub (recurseMultipleListFiles fli)
  lift $ mapM constructCompileFile $ nubBy fileNameEquals files
  
fileNameEquals :: FilePath -> FilePath -> Bool
fileNameEquals f1 f2 = takeFileName f1 == takeFileName f2

-- | Build an individual CompileFile. 
constructCompileFile :: FilePath -> IO CompileFile      
constructCompileFile fp = do
  hasMain <- isFileContainingMain fp
  hasTH <- isFileContainingTH fp
  return $ CompileFile fp hasMain hasTH

-- | Merge source dirs informations from the CompileInput list to 
-- create a single CompileInput
mergeCompileInputsSources :: [CompileInput] -> CompileInput
mergeCompileInputsSources [] = mempty
mergeCompileInputsSources listCpIns = mconcat listCpIns 

-- | Configure the CompileInput with all haskell files configured as targets
setAllHsFilesAsTargets :: CompileInput -> CompileR CompileInput
setAllHsFilesAsTargets cpIn = do
  files <- ask
  return cpIn {cfTargetFiles = map cfFp files }

removeFileWithTemplateHaskell :: CompileInput ->CompileR CompileInput
removeFileWithTemplateHaskell = removeFileWithPredicate cfHasTH

removeFileWithMain :: CompileInput -> CompileR CompileInput
removeFileWithMain = removeFileWithPredicate cfHasMain

removeFileWithPredicate :: (CompileFile -> Bool) -> CompileInput -> CompileR CompileInput
removeFileWithPredicate predicate cpIn = do 
  cpFl <- ask 
  let toRemove = map cfFp $ filter predicate cpFl
  return $ cpIn {cfTargetFiles =  targets \\ toRemove}
  where targets = cfTargetFiles cpIn

-- | Fill compile input with every haskell files in the project except those
-- containing main and template haskell
fillCompileInputWithStandardTarget :: CompileInput -> CompileR CompileInput 
fillCompileInputWithStandardTarget cpIn = setAllHsFilesAsTargets cpIn >>= removeFileWithMain >>=removeFileWithTemplateHaskell

getFullCompileCompileInput :: Shaker IO [CompileInput]
getFullCompileCompileInput = do
  cpIn <- fmap mergeCompileInputsSources  (asks shakerCompileInputs )
  cfFlList <- constructCompileFileList 
  let (mainFiles, nonMainFiles) = partition cfHasMain cfFlList
  let libraries = runReader (setAllHsFilesAsTargets cpIn) nonMainFiles
  let executables = map (runReader (setAllHsFilesAsTargets cpIn) ) $ split mainFiles 
  return $ libraries : executables
  where split = groupBy (\_ _ -> False) 

getFullCompileCompileInputNonMain :: Shaker IO CompileInput
getFullCompileCompileInputNonMain = do
  cpIn <- fmap mergeCompileInputsSources  (asks shakerCompileInputs )
  cfFlList <- constructCompileFileList 
  let (_, nonMainFiles) = partition cfHasMain cfFlList
  let libraries = runReader (setAllHsFilesAsTargets cpIn) nonMainFiles
  return libraries 
