module Shaker.Reflexivite (
  RunnableFunction(..)
  -- * Collect module information functions
  ,runFunction
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
import Distribution.InstalledPackageInfo
import Distribution.Simple.PackageIndex
import DynFlags
import GHC
import GHC.Paths
import Language.Haskell.TH
import Shaker.Action.Compile
import Shaker.GhcInterface
import Shaker.ModuleData
import Shaker.Type 
import Unsafe.Coerce

data RunnableFunction = RunnableFunction {
  runnableFunctionModule :: [String]
  ,runnableLibrairies :: [String]
  ,runnableFunctionFunction :: String -- The function name. Should have IO() as signature
}
 deriving Show

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

-- | List all test group of the project.
-- see "Shaker.TestTH" 
listAllTestFrameworkGroupList :: ShakerInput -> ExpQ
listAllTestFrameworkGroupList = shakerModuleData >>> removeNonTestModules >>> listTestFrameworkGroupList 

-- | List all test group for test-framework from the list of modules
listTestFrameworkGroupList :: [ModuleData] -> ExpQ
listTestFrameworkGroupList = return . ListE . map getSingleTestFrameworkGroup

-- * Test framework integration 

-- | Generate a test group for a given module
getSingleTestFrameworkGroup :: ModuleData -> Exp
getSingleTestFrameworkGroup moduleData = foldl1 AppE [process_to_group_exp, test_case_tuple_list, list_assertion, list_prop]
  where process_to_group_exp = AppE (VarE .mkName $ "processToTestGroup") (LitE (StringL $ moduleDataName moduleData))
        list_prop = ListE $ map getSingleFrameworkQuickCheck $ moduleDataProperties moduleData
        list_assertion = ListE $ map getSingleFrameworkHunit $ moduleDataAssertions moduleData
        test_case_tuple_list = convertHunitTestCaseToTuples (moduleDataTestCase moduleData)

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

