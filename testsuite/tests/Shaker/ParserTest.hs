module Shaker.ParserTest
 where

import Control.Arrow 

import Test.QuickCheck
import Shaker.Type
import Shaker.Parser
import Shaker.Config
import Shaker.PluginConfig
import Data.Map (toList)
import Data.Char
 
prop_parseDefaultAction :: String -> Bool
prop_parseDefaultAction act = either (\_ -> True) (\_ -> False) res
  where res = parseCommand ('x' :act) defaultInput 

prop_parseCommand :: CommandString -> Bool
prop_parseCommand (CommandString str expCom) = either (\_ -> False) (== expCom) res
  where res = parseCommand str defaultInput

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

constructActionString :: (String, ShakerAction) -> ActionString
constructActionString (key, value) = ActionString key (Action value)

instance Arbitrary ActionString where
  arbitrary = do 
        str <- listOf $ elements ['a'..'z'] 
        proc str 
        where 
              -- Build action string without arg
              proc "" = oneof [
                         elements $ map (constructActionString . first (map toUpper)) listCommandMap 
                         ,elements $ map constructActionString listCommandMap
                        ]
              -- build action string with args
              proc str = elements $ map (\(key,value) -> ActionString (key ++ " " ++ trim str) (ActionWithArg value str)  ) listCommandMap 
              listCommandMap = toList defaultCommandMap 

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse

instance Arbitrary CommandString where
  arbitrary = do 
    dur <- elements [Continuous,OneShot]
    spaces <- listOf $ elements " "
    actionString_list <- listOf1 arbitrary
    elements [
        CommandString { comStr = getStringFromDurationAndAction dur actionString_list
          ,command = Command dur (map cfAction actionString_list) }
        , CommandString spaces $ Command OneShot [Action Empty]
        ]

getStringFromDurationAndAction :: Duration -> [ActionString] -> String
getStringFromDurationAndAction dur acts  =
     foldl (\a b-> a ++ " " ++ cfActionStr b ++ " " ) (seed dur) acts
     where seed Continuous = "~"
           seed _ = ""
