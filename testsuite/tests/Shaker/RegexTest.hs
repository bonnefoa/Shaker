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

prop_fileListenInfoIncludeAll :: [String] -> Bool
prop_fileListenInfoIncludeAll list = processListWithRegexp list [] [".*"] == nub list

