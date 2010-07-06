module Shaker.CliTest
 where

import Shaker.Cli
import Shaker.Config
import Shaker.Type
import Shaker.Properties

prop_completeWord :: Action -> Bool
prop_completeWord act = length proposedActions == 1
  where proposedActions = listActions defaultInput (show act)
  
prop_partialWords :: ActionInt -> Bool
prop_partialWords (ActionInt act num) = length proposedActions > 0
  where proposedActions = listActions defaultInput (take num strAct)
        strAct = show act 


