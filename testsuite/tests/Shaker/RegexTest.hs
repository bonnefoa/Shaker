module Shaker.RegexTest
 where
import Shaker.Regex
import Data.List 

prop_filterListAll :: [String] ->Bool
prop_filterListAll list = processListWithRegexp list [".*"] [] == []

prop_filterListNone :: [String] ->Bool
prop_filterListNone list = processListWithRegexp list [] [] == list

prop_filterListPartial :: [String] -> Bool
prop_filterListPartial list = length list >= length filtered
        where filtered = processListWithRegexp list ["[a-z0-9]*"] []

prop_includeAll :: [String] -> Bool
prop_includeAll list = processListWithRegexp list [] [".*"] == nub list

prop_getIncludedAll_all :: [String] -> Bool
prop_getIncludedAll_all list = getIncluded list [".*"] == list

prop_getIncludedAll_none :: [String] -> Bool
prop_getIncludedAll_none list = getIncluded list [] == []

