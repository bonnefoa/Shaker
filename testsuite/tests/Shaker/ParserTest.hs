module Shaker.ParserTest
 where

import Test.QuickCheck
import Shaker.Type
import Shaker.Parser
import Shaker.Config
import Data.Map (toList)
import Data.List (foldl)
 
prop_parseDefaultAction :: String -> Bool
prop_parseDefaultAction act = res == Command OneShot [Help]
  where res = parseCommand defaultInput (act ++"x")

prop_parseCommand :: CommandString -> Bool
prop_parseCommand (CommandString str expCom) = parsed == expCom 
  where parsed = parseCommand defaultInput str

-- * Arbitrary instances 

-- | Command with corresponding string to be parsed
data CommandString = CommandString {
        comStr :: String 
        ,command::Command 
} deriving (Show)
-- | Action paired with expected string to be parsed
data ActionString = ActionString {
       cfActionStr :: String 
       ,cfAction :: Action
} deriving (Show)

instance Arbitrary ActionString where
  arbitrary = do 
    elements $ map (\(a,b) -> ActionString a b) (toList defaultCommandMap)

instance Arbitrary CommandString where
  arbitrary = do 
    dur <- elements [Continuous,OneShot]
    actionStrings <- arbitrary
    return $ CommandString {
        comStr = getStringFromDurationAndAction dur actionStrings
        ,command = Command dur (map cfAction actionStrings) 
    }

getStringFromDurationAndAction :: Duration -> [ActionString] -> String
getStringFromDurationAndAction dur acts  =
     foldl (\a b-> a ++ (cfActionStr b) ) (seed dur) acts
     where seed Continuous = "~"
           seed _ = ""
