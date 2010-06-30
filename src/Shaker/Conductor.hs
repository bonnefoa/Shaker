module Shaker.Conductor
  where

import Shaker.Type
import Shaker.Parser

parseAndExecute :: String -> IO()
parseAndExecute cmd = putStrLn "ga"
 where (Command tp act) = parseCommand cmd


