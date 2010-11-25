module Shaker.CommonUtilTest
 where

import Shaker.CommonUtil
import Control.Arrow
import Data.List

prop_separateEqualShouldNotSplitNubList :: [Int] -> Bool
prop_separateEqualShouldNotSplitNubList lst | null lst = separateEqual lst == []
                                            | otherwise = (nub >>> separateEqual >>> head) lst == nub lst
  
prop_separateEqualShouldSplitListWithEqualTerms :: [Int] -> Bool
prop_separateEqualShouldSplitListWithEqualTerms lst | null lst = True
                                                    | otherwise = all isContainingUnique (separateEqual lst)

isContainingUnique :: Eq a => [a] -> Bool
isContainingUnique lst = length lst == (nub >>> length) lst

