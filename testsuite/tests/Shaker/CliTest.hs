module Shaker.CliTest
 where

import Control.Monad
import Test.QuickCheck 
import Shaker.Type
import Shaker.Cli
import Shaker.Config
import Shaker.Properties()
import System.Console.Haskeline.Completion

instance Arbitrary Action where
  arbitrary = elements [Load,Compile,QuickCheck,Quit,Help]

data ActionInt = ActionInt Action Int
  deriving (Show)
instance Arbitrary ActionInt where
  arbitrary = ActionInt `liftM` arbitrary
                        `ap` elements [2..3]

prop_completeWord :: Action -> Bool
prop_completeWord act = length proposedActions == 1
  where proposedActions =  replacement $ listActions defaultInput (show act)
  
prop_partialWords :: ActionInt -> Bool
prop_partialWords (ActionInt act num) = length proposedActions > 1
  where proposedActions = listActions defaultInput (take num strAct)
        strAct = show act 

prop_completeMultipleWords :: [Action] -> Bool
prop_completeMultipleWords acts = length proposedActions == 1
  where proposedActions = listActions defaultInput cliInput
        cliInput = unwords (map show acts)

prop_partialMultipleWords :: [Action] -> ActionInt -> Bool
prop_partialMultipleWords acts (ActionInt act num) = length proposedActions > 1
  where proposedActions = listActions defaultInput cliInput
        cliInput = unwords (map show acts) ++ " " ++ (take num strAct)
        strAct = show act 

