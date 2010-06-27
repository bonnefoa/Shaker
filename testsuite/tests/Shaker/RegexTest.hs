module Shaker.RegexTest
 where
import Shaker.Regex
import Test.QuickCheck
import Data.List 

prop_filterListAll list = processListWithRegexp list [".*"] [] == []
prop_filterListNone list = processListWithRegexp list [] [] == list
prop_filterListPartial list = length list >= length filtered
        where filtered = processListWithRegexp list ["[a-z0-9]*"] []

prop_includeAll list = processListWithRegexp list [] [".*"] == nub list
prop_getIncludedAll_all list = getIncluded list [".*"] == list
prop_getIncludedAll_none list = getIncluded list [] == []

