module Shaker.TestHelper (
    processToTestGroup
  ) where

import Data.Maybe

import Test.HUnit as H
import Test.Framework.Providers.API as T (Test, testGroup)
import Test.Framework.Providers.HUnit 

convertTestCaseToTestFrameworkTestCase :: (String, H.Test) -> Maybe T.Test
convertTestCaseToTestFrameworkTestCase (name, TestCase assertion) = Just $ testCase name assertion
convertTestCaseToTestFrameworkTestCase _ = Nothing

processToTestGroup :: String -> [(String, H.Test)] -> [T.Test] -> [T.Test] -> T.Test
processToTestGroup testName testCaseList assertionList propertyList = testGroup testName $ concat [listHunitTestCase, assertionList, propertyList]
    where listHunitTestCase = mapMaybe convertTestCaseToTestFrameworkTestCase testCaseList
    
