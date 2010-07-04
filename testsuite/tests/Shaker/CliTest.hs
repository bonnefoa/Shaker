module Shaker.CliTest
 where

import Shaker.Cli
import Shaker.Config
import Shaker.Type
import Test.QuickCheck 
import Shaker.Properties

prop_completeWord :: Action -> Bool
prop_completeWord act = length proposedActions == 1
  where proposedActions = (listActions defaultInput) (show act)
  
prop_partialWords :: ActionInt -> Property
prop_partialWords (ActionInt act num) = num < length strAct ==> length proposedActions > 0
  where proposedActions = (listActions defaultInput) (take num $ strAct)
        strAct = show act 

