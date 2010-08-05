module Shaker.ParserTest
 where

import Test.QuickCheck
import Shaker.Type
import Shaker.Parser
import Shaker.Config
import Shaker.PluginConfig
import Data.Map (toList)
import Data.Char
 
prop_parseDefaultAction :: String -> Bool
prop_parseDefaultAction act = res == Command OneShot [Action Help]
  where res = parseCommand defaultInput ('x' :act )

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
        str <- listOf $ elements ['a'..'z'] 
        proc str 
        where proc "" = elements $ map (\(key,value) -> ActionString key (Action value)) (toList defaultCommandMap)
              proc str = elements $ map (\(key,value) -> ActionString (key ++ " " ++ trim str) (ActionWithArg value str)  ) (toList defaultCommandMap)

trim :: String -> String
trim = reverse . (dropWhile isSpace) . reverse

instance Arbitrary CommandString where
  arbitrary = do 
    dur <- elements [Continuous,OneShot]
    actionStrings <- listOf1 arbitrary
    return CommandString {
        comStr = getStringFromDurationAndAction dur actionStrings
        ,command = Command dur (map cfAction actionStrings) 
    }

getStringFromDurationAndAction :: Duration -> [ActionString] -> String
getStringFromDurationAndAction dur acts  =
     foldl (\a b-> a ++ " " ++ cfActionStr b ++ " " ) (seed dur) acts
     where seed Continuous = "~"
           seed _ = ""
