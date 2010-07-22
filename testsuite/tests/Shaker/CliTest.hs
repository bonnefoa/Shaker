module Shaker.CliTest
 where

import Control.Monad
import Test.QuickCheck 
import Test.QuickCheck.Monadic
import Shaker.Type
import Shaker.Cli
import Shaker.Config
import Shaker.Properties()
import System.Console.Haskeline.Completion

instance Arbitrary Action where
  arbitrary = elements [Compile,Quit,Help]

data ActionInt = ActionInt Action Int
  deriving (Show)
instance Arbitrary ActionInt where
  arbitrary = ActionInt `liftM` arbitrary
                        `ap` elements [1..6]

checkRes :: Monad m => String -> String -> PropertyM m ()
checkRes incomplete expected = do
  proposedActions <- listActions defaultInput incomplete
  assert $ any (\a -> replacement a == expected) proposedActions

prop_completeWord :: Action -> Property
prop_completeWord act = monadicIO $ checkRes str str 
  where str = show act

prop_partialWords :: ActionInt -> Property
prop_partialWords (ActionInt act num) = monadicIO $ checkRes incomplete strAct 
  where strAct = show act 
        incomplete = take num strAct

prop_completeMultipleWords :: [Action] -> Property
prop_completeMultipleWords acts = (not . null) acts ==> monadicIO $ checkRes cliInput cliInput 
  where cliInput = unwords (map show acts)

prop_partialMultipleWords :: [Action] -> ActionInt -> Property
prop_partialMultipleWords acts (ActionInt act num) = (not . null) acts ==> monadicIO $ checkRes cliInput expected
  where cliInput = strActs ++ " " ++ take num strAct
        strAct = show act 
        strActs =unwords $ map show acts 
        expected = unwords [strActs,strAct] 

