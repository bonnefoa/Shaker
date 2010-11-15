module Shaker.Reflexivite (
  ModuleMapping(..)
  ,RunnableFunction(..)
  -- * Collect module information functions
  ,collectAllModulesForTest
  ,collectChangedModulesForTest 
  ,runFunction
  ,removeNonTestModule
  ,searchInstalledPackageId
  -- * Template haskell generator
  ,listAllTestFrameworkGroupList
  ,filterModulesWithPattern
  ,filterFunctionsWithPatterns
  ,listTestFrameworkGroupList 
  )
 where

import Control.Arrow
import Control.Exception as C
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Digraph
import Distribution.InstalledPackageInfo
import Distribution.Simple.PackageIndex
import DynFlags
import GHC
import GHC.Paths
import Language.Haskell.TH
import Name (nameOccName)
import OccName (occNameString)
import Outputable
import Shaker.Action.Compile
import Shaker.GhcInterface
import Shaker.ModuleData
import Shaker.Regex
import Shaker.Type 
import Unsafe.Coerce
import Var (varName)

-- | Mapping between module name (to import) and test to execute
data ModuleMapping = ModuleMapping {
  runnableFunctionModuleName :: String -- ^ Complete name of the module 
  ,moduleMappingHunitAssertion :: [String] -- ^ Hunit assertions
  ,moduleMappingHunitTestCase :: [String] -- ^ Hunit test case to process for test-framework
  ,moduleMappingPropName :: [String] -- ^ QuickCheck test function names
 }
 deriving (Show,Eq)

data RunnableFunction = RunnableFunction {
  runnableFunctionModule :: [String]
  ,runnableLibrairies :: [String]
  ,runnableFunctionFunction :: String -- The function name. Should have IO() as signature
}
 deriving Show

-- | Collect all non-main modules with their test function associated
collectAllModulesForTest :: Shaker IO [ModuleMapping]
collectAllModulesForTest = do 
  cpInList <- convertModuleDataToFullCompileInput
  allModules <- fmap concat (lift $ mapM collectModules cpInList)
  return . removeNonTestModule $ allModules
  where collectModules cpIn = 
          runGhc (Just libdir) $ ghcCompile cpIn >> collectAllModules' >>= mapM getModuleMapping  

-- | Analyze all haskell modules of the project and 
-- output all module needing recompilation
collectChangedModulesForTest :: Shaker IO [ModuleMapping]
collectChangedModulesForTest = do 
  cpInList <- convertModuleDataToFullCompileInput
  modInfoFiles <- asks shakerModifiedInfoFiles
  let modFilePaths = map fileInfoFilePath modInfoFiles
  changed_modules <- fmap concat (lift $ mapM (collectChangedModules modFilePaths) cpInList) 
  return . removeNonTestModule $ changed_modules
  where collectChangedModules modFilePaths cpIn = 
          runGhc (Just libdir) $ initializeGhc cpIn >> collectChangedModulesForTest' modFilePaths cpIn


collectAllModules' :: GhcMonad m => m [ModSummary] 
collectAllModules' = do 
  mss <- depanal [] False
  let sort_mss = flattenSCCs $ topSortModuleGraph True mss Nothing
  return sort_mss
         
collectChangedModulesForTest' :: GhcMonad m => [FilePath] -> CompileInput -> m [ModuleMapping] 
collectChangedModulesForTest' modFilePaths cpIn = do 
    changedModules <- collectAllModules' >>= filterM (isModuleNeedCompilation modFilePaths) 
    _ <- ghcCompile cpIn
    allModules <- collectAllModules' 
    let res = intersectBy ( \a b -> nameMod a == nameMod b ) allModules changedModules
    mapM getModuleMapping res
  where nameMod = moduleNameString . moduleName . ms_mod

-- | Compile, load and run the given function
runFunction :: RunnableFunction -> Shaker IO()
runFunction (RunnableFunction importModuleList listLibs fun) = do
  cpIn <- getNonMainCompileInput
  listInstalledPkgId <- fmap catMaybes (mapM searchInstalledPackageId listLibs)
  dynFun <- lift $ runGhc (Just libdir) $ do
         dflags <- getSessionDynFlags
         _ <- setSessionDynFlags (addShakerLibraryAsImport listInstalledPkgId (dopt_set dflags Opt_HideAllPackages))
         _ <- ghcCompile cpIn 
         configureContext importModuleList
         value <- compileExpr fun
         do let value' = unsafeCoerce value :: a
            return value'
  _ <- lift $ handleActionInterrupt dynFun
  return ()
  where 
        configureContext [] = getModuleGraph >>= \mGraph ->  setContext [] $ map ms_mod mGraph
        configureContext imports = mapM (\a -> findModule (mkModuleName a)  Nothing ) imports >>= \m -> setContext [] m

addShakerLibraryAsImport :: [String] -> DynFlags -> DynFlags
addShakerLibraryAsImport listInstalledPkgId dflags = dflags {
    packageFlags = nub $ map ExposePackageId listInstalledPkgId ++ oldPackageFlags
  }
  where oldPackageFlags = packageFlags dflags

searchInstalledPackageId :: String -> Shaker IO (Maybe String)
searchInstalledPackageId pkgName = do 
  pkgIndex <- asks shakerPackageIndex
  let srchRes = searchByName pkgIndex pkgName 
  return $ processSearchResult srchRes
  where processSearchResult None = Nothing
        processSearchResult (Unambiguous a) = Just $ installedPackageId >>> installedPackageIdString $ head a
        processSearchResult (Ambiguous (a:_)) = Just $ installedPackageId >>> installedPackageIdString $ head a
        processSearchResult _ = Nothing

handleActionInterrupt :: IO() -> IO()
handleActionInterrupt =  C.handle catchAll
  where catchAll :: C.SomeException -> IO ()
        catchAll e = putStrLn ("Shaker caught " ++ show e ) >>  return () 

-- | Collect module name and tests name for the given module
getModuleMapping :: (GhcMonad m) => ModSummary -> m ModuleMapping
getModuleMapping  modSum = do 
  mayModuleInfo <- getModuleInfo $  ms_mod modSum
  let props = getQuickCheckFunction mayModuleInfo
  let hunits = getHunitAssertions mayModuleInfo
  let testCases = getHunitTestCase mayModuleInfo
  return $ ModuleMapping modName hunits testCases props
  where modName = (moduleNameString . moduleName . ms_mod) modSum        
       
getQuickCheckFunction :: Maybe ModuleInfo -> [String]
getQuickCheckFunction = getFunctionNameWithPredicate ("prop_" `isPrefixOf`) 

getHunitAssertions :: Maybe ModuleInfo -> [String]
getHunitAssertions = getFunctionTypeWithPredicate (== "Test.HUnit.Lang.Assertion") 

getHunitTestCase :: Maybe ModuleInfo -> [String]
getHunitTestCase = getFunctionTypeWithPredicate (== "Test.HUnit.Base.Test") 

getFunctionTypeWithPredicate :: (String -> Bool) -> Maybe ModuleInfo -> [String]
getFunctionTypeWithPredicate _ Nothing = []
getFunctionTypeWithPredicate predicat (Just modInfo) = map snd $ filter ( predicat . fst)  typeList
   where idList = getIdList modInfo
         typeList = map ((showPpr . idType) &&& getFunctionNameFromId ) idList 

getFunctionNameWithPredicate :: (String -> Bool) -> Maybe ModuleInfo -> [String]
getFunctionNameWithPredicate _ Nothing = []
getFunctionNameWithPredicate predicat (Just modInfo) = 
  filter predicat nameList
   where idList = getIdList modInfo
         nameList = map getFunctionNameFromId idList 

getFunctionNameFromId :: Id -> String
getFunctionNameFromId = occNameString . nameOccName . varName

getIdList :: ModuleInfo -> [Id]
getIdList modInfo = mapMaybe tyThingToId $ modInfoTyThings modInfo

tyThingToId :: TyThing -> Maybe Id
tyThingToId (AnId tyId) = Just tyId
tyThingToId _ = Nothing

-- | List all test group of the project.
-- see "Shaker.TestTH" 
listAllTestFrameworkGroupList :: ShakerInput -> ExpQ
listAllTestFrameworkGroupList shIn = runIO (runReaderT collectAllModulesForTest shIn) >>= listTestFrameworkGroupList . removeNonTestModule

-- | List all test group for test-framework from the list of modules
listTestFrameworkGroupList :: [ModuleMapping] -> ExpQ
listTestFrameworkGroupList = return . ListE . map getSingleTestFrameworkGroup

-- | Remove all modules which does not contain test
removeNonTestModule :: [ModuleMapping] -> [ModuleMapping]
removeNonTestModule = filter (\modMap -> notEmpty (moduleMappingHunitAssertion modMap) || notEmpty (moduleMappingPropName modMap) || notEmpty (moduleMappingHunitTestCase modMap) )
  where notEmpty = not.null

-- * Test framework integration 

-- | Generate a test group for a given module
getSingleTestFrameworkGroup :: ModuleMapping -> Exp
getSingleTestFrameworkGroup modMap = foldl1 AppE [process_to_group_exp, test_case_tuple_list, list_assertion, list_prop]
  where process_to_group_exp = AppE (VarE .mkName $ "processToTestGroup") (LitE (StringL $ runnableFunctionModuleName modMap))
        list_prop = ListE $ map getSingleFrameworkQuickCheck $ moduleMappingPropName modMap
        list_assertion = ListE $ map getSingleFrameworkHunit $ moduleMappingHunitAssertion modMap
        test_case_tuple_list = convertHunitTestCaseToTuples (moduleMappingHunitTestCase modMap)

convertHunitTestCaseToTuples :: [String] -> Exp
convertHunitTestCaseToTuples = ListE . map convertToTuple 
  where convertToTuple name = TupE [LitE (StringL name), VarE $ mkName name ]

-- | Generate an expression for a single hunit test
getSingleFrameworkHunit :: String -> Exp 
getSingleFrameworkHunit hunitName = AppE testcase_with_name_exp assertion_exp
  where testcase_with_name_exp = AppE ( VarE $ mkName "testCase") (LitE $ StringL hunitName)
        assertion_exp = VarE . mkName $ hunitName

-- | Generate an expression for a single quickcheck property
getSingleFrameworkQuickCheck :: String -> Exp
getSingleFrameworkQuickCheck propName = AppE testproperty_with_name_exp property_exp 
  where canonical_name = tail . dropWhile (/= '_') $ propName 
        testproperty_with_name_exp = AppE ( VarE $ mkName "testProperty") (LitE $ StringL canonical_name)
        property_exp = VarE . mkName $ propName

-- * utility functions 

-- | Include only module matching the given pattern
filterModulesWithPattern :: [ModuleMapping]-> String -> [ModuleMapping]
filterModulesWithPattern mod_map pattern = filter (\a -> runnableFunctionModuleName a `elem` filtered_mod_list) mod_map
  where mod_list = map runnableFunctionModuleName mod_map
        filtered_mod_list = processListWithRegexp mod_list [] [pattern]

filterFunctionsWithPatterns :: [ModuleMapping] -> [String] -> [ModuleMapping]
filterFunctionsWithPatterns mod_map patterns = map (`filterFunctionsWithPatterns'` patterns) mod_map

filterFunctionsWithPatterns' :: ModuleMapping -> [String] -> ModuleMapping
filterFunctionsWithPatterns' (ModuleMapping name hunitAssertions hunitTestCases properties) patterns = 
  ModuleMapping{
    runnableFunctionModuleName = name
    ,moduleMappingHunitAssertion = processListWithRegexp hunitAssertions [] patterns
    ,moduleMappingHunitTestCase = processListWithRegexp hunitTestCases [] patterns
    ,moduleMappingPropName = processListWithRegexp properties [] patterns
  }


