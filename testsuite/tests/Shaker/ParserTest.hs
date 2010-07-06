module Shaker.ParserTest
 where

import Test.QuickCheck
import Shaker.Type
import Shaker.Parser
import Shaker.Config
import Text.ParserCombinators.Parsec


prop_parseTypeOneShot :: String -> Bool
prop_parseTypeOneShot cont = checkRes res (== OneShot)
  where res = parse typeDuration "test" (filter (/= '~') cont)

prop_parseTypeContinuous :: Int -> Property
prop_parseTypeContinuous num= num <1000 ==> checkRes res (== Continuous)
  where res = parse typeDuration "test" (replicate num ' '++"~" )

prop_parseDefaultAction :: String -> Bool
prop_parseDefaultAction act = res == Command OneShot Help  
  where res = parseCommand defaultInput (act ++"x")

prop_parseAction :: Action -> Bool
prop_parseAction act = checkRes res (==act)  
  where res = parse (typeAction defaultCommandMap) "test" (show act)

prop_parseOneShotCommand :: Action -> Bool
prop_parseOneShotCommand act = checkRes res (== Command OneShot act)
  where res = parse (typeCommand defaultCommandMap) "test" (show act)

prop_parseContinuousCommand :: Action -> Bool
prop_parseContinuousCommand act = checkRes res (== Command Continuous act)
  where res = parse (typeCommand defaultCommandMap) "test" ('~':show act)

checkRes :: (Either ParseError a) -> (a -> Bool) -> Bool
checkRes (Left _) _ = False
checkRes (Right val) predica = predica val


