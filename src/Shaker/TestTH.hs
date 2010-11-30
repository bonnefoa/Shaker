-- | Allow to dynamically construct a list of 
-- quickcheck properties and Hunit test with template haskell
module Shaker.TestTH
 where

import Control.Arrow
import Language.Haskell.TH
import Shaker.ModuleData
import Shaker.Type
import Shaker.TestHelper

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

