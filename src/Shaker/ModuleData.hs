module Shaker.ModuleData
 where

import Control.Arrow
import Control.Monad.Reader
import Data.List 
import Data.Monoid
import Language.Haskell.Syntax
import Shaker.HsHelper
import Shaker.Io
import Shaker.Type
import Shaker.Regex
import System.Directory
import System.FilePath

writeModuleData :: ModuleData -> Shaker IO ()
writeModuleData moduleData = do
  let srcFile = moduleDataFileName moduleData
  buildFile <- fmap (`addExtension` moduleDataExtension) (getCorrespondingBuildFile srcFile)
  lift $ createDirectoryIfMissing True (dropFileName buildFile) 
  lift $ writeFile buildFile (show moduleData) 

readModuleDataIfExist :: FilePath -> Shaker IO (Maybe ModuleData)
readModuleDataIfExist srcFile = do
  buildFile <- fmap (`addExtension` moduleDataExtension) (getCorrespondingBuildFile srcFile)
  exist <- lift $ doesFileExist buildFile
  if exist 
    then lift $ fmap Just (parseFileToModuleData buildFile)
    else return Nothing

parseFileToModuleData :: FilePath -> IO ModuleData
parseFileToModuleData src = fmap read (readFile src)

parseModuleData :: [FileListenInfo] -> IO [ ModuleData ]
parseModuleData mods = fmap (map constructModuleData) (parseHsFiles mods)

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

