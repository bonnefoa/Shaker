module Shaker.ParserTest
 where

import Test.QuickCheck
import Shaker.Type
import Shaker.Parser
import Shaker.Config
import Data.Map (toList)
 
prop_parseDefaultAction :: String -> Bool
prop_parseDefaultAction act = res == Command OneShot Help  
  where res = parseCommand defaultInput (act ++"x")

prop_parseCommand :: CommandString -> Bool
prop_parseCommand (CommandString str expCom) = parsed == expCom 
  where parsed = parseCommand defaultInput str

instance Arbitrary Duration where
  arbitrary = elements [Continuous,OneShot]

data CommandString = CommandString String Command 
 deriving (Show)
instance Arbitrary CommandString where
  arbitrary = do 
    dur <- arbitrary 
    elements $ map (\(a,b) -> CommandString (appendDur dur a) (Command dur b))  $ toList defaultCommandMap 

appendDur :: Duration -> String -> String
appendDur Continuous str = "~" ++ str
appendDur _ str = str
