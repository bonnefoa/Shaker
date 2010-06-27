module Shaker.Regex
where

import Text.Regex.Posix
import Data.List

processListWithRegexp :: [String] -> [String] -> [String] -> [String]
processListWithRegexp list [] [] = list
processListWithRegexp list ignore include = 
  nub $ (getExcluded list ignore) ++ (getIncluded list include)

getExcluded :: [String] -> [String] -> [String]
getExcluded list patterns = filter funExclude list
 where funExclude el = not $ any (el =~) patterns

getIncluded :: [String] -> [String] -> [String]
getIncluded list patterns = filter funInclude list
  where funInclude el = any (el =~) patterns
