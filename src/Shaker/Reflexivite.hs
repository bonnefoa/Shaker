module Shaker.Reflexivite(
  ModuleMapping(..)
  ,RunnableFunction(..)
  -- * Collect module information functions
  ,collectAllModulesForTest
  ,collectAllModules
  ,collectChangedModules
  ,collectChangedModulesForTest 
  ,runFunction
  -- * Template haskell generator
  ,listHunit
  ,listProperties
  ,listAllProperties
  ,listAllHunit
  ,filterModulesWithPattern
  ,listTestFrameworkGroupList 
)
 where

import Data.List
import Data.Maybe
import Shaker.Type 
import Shaker.Action.Compile
import Shaker.SourceHelper
import Shaker.Regex

import Control.Monad.Reader(runReader,runReaderT,asks, Reader, lift, filterM)
import Control.Arrow

import Digraph
import Language.Haskell.TH
import GHC
import GHC.Paths
import Unsafe.Coerce
import Outputable
import OccName (occNameString)
import Name (nameOccName)
import Var (varName)

-- | Mapping between module name (to import) and test to execute
data ModuleMapping = ModuleMapping {
  cfModuleName :: String -- ^ Complete name of the module 
  ,cfHunitName :: [String] -- ^ Hunit test function names
  ,cfPropName :: [String] -- ^ QuickCheck test function names
 }
 deriving (Show,Eq)

data RunnableFunction = RunnableFunction {
  cfModule :: [String]
  ,cfFunctionName :: String -- The function name. Should have IO() as signature
}
 deriving Show

initializeFilesForCompilation :: Shaker IO (CompileInput, [CompileFile] )
initializeFilesForCompilation = do
  cpList <- asks compileInputs 
  let cpIn = mergeCompileInputsSources cpList
  cfFlList <- lift $ constructCompileFileList cpIn
  return (cpIn, cfFlList)

-- | Collect all non-main modules with their test function associated
collectAllModulesForTest :: Shaker IO [ModuleMapping]
collectAllModulesForTest = do 
  (cpIn, cfFlList) <- initializeFilesForCompilation 
  lift $ runGhc (Just libdir) $ do 
        _ <- ghcCompile $ runReader (fillCompileInputWithStandardTarget cpIn) cfFlList
        collectAllModules' >>= mapM getModuleMapping 

-- | Collect all non-main modules 
collectAllModules :: Shaker IO [ModSummary]
collectAllModules = do
  (cpIn, cfFlList) <- initializeFilesForCompilation 
  lift $ runGhc (Just libdir) $ do
        _ <- ghcCompile $ runReader (fillCompileInputWithStandardTarget cpIn) cfFlList
        collectAllModules'

-- | Analyze all haskell modules of the project and 
-- output all module needing recompilation
collectChangedModules :: Shaker IO [ModSummary]
collectChangedModules = do 
  (cpIn, cfFlList) <- initializeFilesForCompilation 
  modInfoFiles <- asks modifiedInfoFiles
  let modFilePaths = (map fileInfoFilePath modInfoFiles)
  lift $ runGhc (Just libdir) $ do 
           _ <- initializeGhc $ runReader (setAllHsFilesAsTargets cpIn >>= removeFileWithMain ) cfFlList
           collectChangedModules' modFilePaths

collectChangedModulesForTest :: Shaker IO [ModuleMapping]
collectChangedModulesForTest = do 
  (cpIn, cfFlList) <- initializeFilesForCompilation 
  modInfoFiles <- asks modifiedInfoFiles
  let modFilePaths = (map fileInfoFilePath modInfoFiles)
  lift $ runGhc (Just libdir) $ do 
           let processedCpIn = runReader (setAllHsFilesAsTargets cpIn >>= removeFileWithMain ) cfFlList
           _ <- initializeGhc processedCpIn
           collectChangedModulesForTest' modFilePaths processedCpIn

collectAllModules' :: GhcMonad m => m [ModSummary] 
collectAllModules' = do 
  mss <- depanal [] False
  let sort_mss = flattenSCCs $ topSortModuleGraph True mss Nothing
  return sort_mss
         

collectChangedModules' :: GhcMonad m => [FilePath] -> m [ModSummary] 
collectChangedModules' modFilePaths = collectAllModules' >>= filterM (isModuleNeedCompilation modFilePaths) 

collectChangedModulesForTest' :: GhcMonad m => [FilePath] -> CompileInput -> m [ModuleMapping] 
collectChangedModulesForTest' modFilePaths cpIn = do 
    changedModules <- collectChangedModules' modFilePaths 
    _ <- ghcCompile cpIn
    allModules <- collectAllModules' 
    let res = intersectBy ( \a b -> nameMod a == nameMod b ) allModules changedModules
    mapM getModuleMapping res
  where nameMod = moduleNameString . moduleName . ms_mod

-- | Compile, load and run the given function
runFunction :: RunnableFunction -> Shaker IO()
runFunction (RunnableFunction funModuleName fun) = do
  (cpIn, cfFlList) <- initializeFilesForCompilation 
  dynFun <- lift $ runGhc (Just libdir) $ do
         _ <- ghcCompile $ runReader (setAllHsFilesAsTargets cpIn >>= removeFileWithMain ) cfFlList
         configureContext funModuleName
         value <- compileExpr fun
         do let value' = unsafeCoerce value :: a
            return value'
  _ <- lift dynFun
  return () 
  where 
        configureContext [] = getModuleGraph >>= \mGraph ->  setContext [] $ map ms_mod mGraph
        configureContext imports = mapM (\a -> findModule (mkModuleName a)  Nothing ) imports >>= \m -> setContext [] m

-- | Collect module name and tests name for the given module
getModuleMapping :: (GhcMonad m) => ModSummary -> m ModuleMapping
getModuleMapping  modSum = do 
  mayModuleInfo <- getModuleInfo $  ms_mod modSum
  let props = getQuickCheckFunction mayModuleInfo
  let hunits = getHunitFunctions mayModuleInfo
  return $ ModuleMapping modName hunits props
  where modName = (moduleNameString . moduleName . ms_mod) modSum        
       
getQuickCheckFunction :: Maybe ModuleInfo -> [String]
getQuickCheckFunction = getFunctionNameWithPredicate ("prop_" `isPrefixOf`) 

getHunitFunctions :: Maybe ModuleInfo -> [String]
getHunitFunctions = getFunctionTypeWithPredicate (== "Test.HUnit.Lang.Assertion") 

getFunctionTypeWithPredicate :: (String -> Bool) -> Maybe ModuleInfo -> [String]
getFunctionTypeWithPredicate _ Nothing = []
getFunctionTypeWithPredicate predicat (Just modInfo) = map snd $ filter ( predicat . fst)  typeList
   where idList = getIdList modInfo
         typeList = map ((showPpr . idType) &&& getFunctionNameFromId ) idList 

getFunctionNameWithPredicate :: (String -> Bool) -> Maybe ModuleInfo -> [String]
getFunctionNameWithPredicate _ Nothing = []
getFunctionNameWithPredicate predicat (Just modInfo) = filter predicat nameList
   where idList = getIdList modInfo
         nameList = map getFunctionNameFromId idList 

getFunctionNameFromId :: Id -> String
getFunctionNameFromId = occNameString . nameOccName . varName

getIdList :: ModuleInfo -> [Id]
getIdList modInfo = mapMaybe tyThingToId $ modInfoTyThings modInfo

tyThingToId :: TyThing -> Maybe Id
tyThingToId (AnId tyId) = Just tyId
tyThingToId _ = Nothing

getSingleQuickCheck :: String -> Exp
getSingleQuickCheck propName = InfixE (Just printName) (VarE $ mkName ">>") (Just quickCall)
  where quickCall = (AppE (VarE $ mkName "quickCheck" ) . VarE . mkName) propName
        printName = AppE (VarE $ mkName "putStrLn") (LitE (StringL propName)) 

listProperties :: [ModuleMapping] -> ExpQ
listProperties modMaps = return $ ListE $ getQuickCheckProperty modMaps
  where getQuickCheckProperty :: [ModuleMapping] -> [Exp]
        getQuickCheckProperty = concatMap (\modMap -> map getSingleQuickCheck $ cfPropName modMap)

-- | List the quickeck properties of the project.
-- see "Shaker.TestTH"
listAllProperties :: ShakerInput -> ExpQ
listAllProperties shIn = runIO (runReaderT collectAllModulesForTest shIn) >>= listProperties

-- | List all test case of the project.
-- see "Shaker.TestTH"
listHunit :: [ModuleMapping] -> ExpQ
listHunit modMaps = return $ ListE $ getHunit modMaps
  where getHunit :: [ModuleMapping] -> [Exp]
        getHunit = concatMap (\modMap -> map (VarE . mkName) $ cfHunitName modMap) 

listAllHunit :: ShakerInput -> ExpQ
listAllHunit shIn = runIO ( runReaderT collectAllModulesForTest shIn ) >>= listHunit

listTestFrameworkGroupList :: [ModuleMapping] -> ExpQ
listTestFrameworkGroupList = return . ListE . map getSingleTestFrameworkGroup

-- * Test framework integration 

getSingleTestFrameworkGroup :: ModuleMapping -> Exp
getSingleTestFrameworkGroup modMap = AppE first_arg second_arg
  where first_arg = AppE (VarE .mkName $ "testGroup") (LitE (StringL $ cfModuleName modMap))
        second_arg = ListE $ list_prop ++ list_hunit 
        list_prop = map getSingleFrameworkQuickCheck $ cfPropName modMap
        list_hunit = map getSingleFrameworkHunit $ cfHunitName modMap

getSingleFrameworkHunit :: String -> Exp 
getSingleFrameworkHunit hunitName = AppE first_arg second_arg 
  where first_arg = AppE ( VarE $ mkName "testCase") (LitE $ StringL hunitName)
        second_arg = VarE . mkName $ hunitName

getSingleFrameworkQuickCheck :: String -> Exp
getSingleFrameworkQuickCheck propName = AppE first_arg second_arg 
  where canonical_name = tail . dropWhile (/= '_') $ propName 
        first_arg = AppE ( VarE $ mkName "testProperty") (LitE $ StringL canonical_name)
        second_arg = VarE . mkName $ propName

-- * utility functions 

-- | Include only module matching the given pattern
filterModulesWithPattern :: Maybe String -> [ModuleMapping] -> [ModuleMapping]
filterModulesWithPattern Nothing mod_map = mod_map
filterModulesWithPattern (Just pattern) mod_map = filter (\a -> cfModuleName a `elem` filtered_mod_list) mod_map
  where mod_list = map cfModuleName mod_map
        filtered_mod_list = processListWithRegexp mod_list [] [pattern]
