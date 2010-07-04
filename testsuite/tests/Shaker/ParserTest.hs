module Shaker.ParserTest
 where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Shaker.Type
import Shaker.Parser
import Shaker.Config
import Shaker.Properties
import Text.ParserCombinators.Parsec
import Control.Monad.Trans


prop_parseTypeOneShot cont = checkRes res (== OneShot)
  where res = parse typeDuration "test" (filter (/= '~') cont)

prop_parseTypeContinuous num= num <1000 ==> checkRes res (== Continuous)
  where res = parse typeDuration "test" ((replicate num ' ')++"~" )

prop_parseDefaultAction act = res == Command OneShot Help  
  where res = parseCommand defaultInput (act ++"x")

prop_parseAction act = checkRes res (==act)  
  where res = parse (typeAction defaultCommandMap) "test" (show act)

prop_parseOneShotCommand act = checkRes res (== Command OneShot act)
  where res = parse (typeCommand defaultCommandMap) "test" (show act)

prop_parseContinuousCommand act = checkRes res (== Command Continuous act)
  where res = parse (typeCommand defaultCommandMap) "test" ("~"++show act)

checkRes (Left _) _ = False
checkRes (Right val) pred = pred val


