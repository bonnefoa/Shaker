module Shaker.RegexTest
 where
import Shaker.Regex
import Test.QuickCheck

prop_filterListAll list = filterListWithRegexp list [".*"] == []
prop_filterListNone list = filterListWithRegexp list [] == list
prop_filterListPartial list = length list >= length filtered
        where filtered = filterListWithRegexp list ["[a-z0-9]*"]

