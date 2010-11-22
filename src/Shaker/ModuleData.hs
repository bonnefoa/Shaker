module Shaker.ModuleData
 where

import Control.Arrow
import Control.Monad.Reader
import Data.List 
import Data.Maybe
import Data.Monoid
import Language.Haskell.Syntax
import Shaker.HsHelper
import Shaker.Io
import Shaker.Type
import Shaker.Regex
import System.Directory
import System.FilePath

-- * Read and write module data

-- | Get the corresponding mdata file from the given source file
getCorrespondingModuleDataFile :: FilePath -> Shaker IO FilePath
getCorrespondingModuleDataFile srcFile = 
  fmap (`addExtension` moduleDataExtension) (getCorrespondingBuildFile srcFile)

-- | Write given moduleData in dist directory
writeModuleData :: ModuleData -> Shaker IO ()
writeModuleData moduleData = do
  let srcFile = moduleDataFileName moduleData
  buildFile <- getCorrespondingModuleDataFile srcFile 
  lift $ createDirectoryIfMissing True (dropFileName buildFile) 
  lift $ writeFile buildFile (show moduleData) 

-- | Parse module data from all haskell sources. 
parseAllModuleData :: Shaker IO [ ModuleData ]
parseAllModuleData = do
  lstHsFiles <- fmap listenerInputFiles (asks shakerListenerInput) >>= lift . recurseMultipleListFiles
  fmap catMaybes $ mapM parseModuleData lstHsFiles

-- | Read Module data from the given haskell source. It tries to read serialized information beforehand.
parseModuleData :: FilePath -> Shaker IO (Maybe ModuleData)
parseModuleData srcFile = do 
  may_moduleData <- parseModuleDataIfExist srcFile 
  case may_moduleData of
    Just _ -> return $ may_moduleData
    Nothing -> do
      may_hsModule <- lift $ parseFileToHsModule srcFile
      return $ fmap constructModuleData may_hsModule 

-- | Read Module data from the serialized data. It returns Nothing if the serialized data is absent or out-of-date.
parseModuleDataIfExist :: FilePath -> Shaker IO (Maybe ModuleData)
parseModuleDataIfExist srcFile = do
  buildFile <- fmap (`addExtension` moduleDataExtension) (getCorrespondingBuildFile srcFile)
  isPresent <- lift $ doesFileExist buildFile 
  case isPresent of
    False -> return Nothing
    True -> lift $ do
           srcTime <- getModificationTime srcFile
           srcMdata <- getModificationTime buildFile
           let isUptoDate = srcTime < srcMdata
           case isUptoDate of
              False -> return Nothing 
              True -> fmap Just $ fmap read (readFile buildFile)

-- * Module data util methods

convertModuleDataToFullCompileInput :: Shaker IO [CompileInput]
convertModuleDataToFullCompileInput = do 
  baseCpIn <- fmap mconcat (asks shakerCompileInputs)
  lstModuleDatas <- asks shakerModuleData
  let groupOfCompileModules = groupByValidTargets lstModuleDatas
  return $ map ( \ lstModules -> baseCpIn { compileInputTargetFiles = map moduleDataFileName lstModules } ) groupOfCompileModules

groupByValidTargets :: [ModuleData] -> [ [ ModuleData] ] 
groupByValidTargets = partition moduleDataHasMain 
  >>> first (groupBy mainGroupPredicate)
  >>> second nub 
  >>> ( \ (a, b) -> b : a ) 
  where mainGroupPredicate _ _ = False

getNonMainCompileInput :: Shaker IO CompileInput
getNonMainCompileInput = do
  baseCpIn <- fmap mconcat (asks shakerCompileInputs)
  lstModuleDatas <- asks shakerModuleData
  let filteredModuleDatas = filter (not . moduleDataHasMain) >>> nub $ lstModuleDatas
  return $ baseCpIn { compileInputTargetFiles = map moduleDataFileName filteredModuleDatas } 

fillModuleData :: ShakerInput -> IO ShakerInput
fillModuleData shIn = do
  lstHsModules <- shakerListenerInput >>> listenerInputFiles >>> parseHsFiles $ shIn
  return shIn { shakerModuleData = map constructModuleData lstHsModules }

constructModuleData :: HsModule -> ModuleData
constructModuleData hsModule = ModuleData {
  moduleDataName = hsModuleName hsModule
  ,moduleDataFileName = hsModuleFileName hsModule
  ,moduleDataHasMain = getTupleFunctionNameType >>> map fst >>> any (=="main") $ hsModule
  ,moduleDataProperties = hsModuleCollectProperties hsModule
  ,moduleDataAssertions = hsModuleCollectAssertions hsModule
  ,moduleDataTestCase = hsModuleCollectTest hsModule
 }

hsModuleDataHasTest :: ModuleData -> Bool
hsModuleDataHasTest hsModuleData = any (not . null) [moduleDataProperties hsModuleData, moduleDataAssertions hsModuleData] 

-- | Include only module matching the given pattern
filterModulesWithPattern :: [ModuleData]-> String -> [ModuleData]
filterModulesWithPattern mod_map pattern = filter (\a -> moduleDataName a `elem` filtered_mod_list) mod_map
  where mod_list = map moduleDataName mod_map
        filtered_mod_list = processListWithRegexp mod_list [] [pattern]

filterFunctionsWithPatterns :: [ModuleData] -> [String] -> [ModuleData]
filterFunctionsWithPatterns mod_map patterns = map (`filterFunctionsWithPatterns'` patterns) mod_map

filterFunctionsWithPatterns' :: ModuleData -> [String] -> ModuleData
filterFunctionsWithPatterns' moduleData@(ModuleData _ _ _ properties hunitAssertions hunitTestCases) patterns = moduleData {
    moduleDataAssertions = processListWithRegexp hunitAssertions [] patterns
    ,moduleDataTestCase = processListWithRegexp hunitTestCases [] patterns
    ,moduleDataProperties = processListWithRegexp properties [] patterns
  }

removeNonTestModules :: [ModuleData] -> [ModuleData]
removeNonTestModules = filter ( \ moduleData -> any notEmpty [moduleDataProperties moduleData, moduleDataAssertions moduleData, moduleDataTestCase moduleData] )
  where notEmpty = not . null

