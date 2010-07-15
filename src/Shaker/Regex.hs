module Shaker.Regex
where

import Text.Regex.Posix
import Data.List

processListWithRegexp :: [String] -> [String] -> [String] -> [String]
processListWithRegexp list [] [] = list
processListWithRegexp list ignore [] = nub $ list \\ getExcluded list ignore 
processListWithRegexp list [] include = nub $ getIncluded list include
processListWithRegexp list ignore include = 
  nub $ getIncluded list include \\ getExcluded list ignore 

getExcluded :: [String] -> [String] -> [String]
getExcluded list patterns = filter funExclude list
 where funExclude el = any (el =~) patterns

getIncluded :: [String] -> [String] -> [String]
getIncluded list patterns = filter funInclude list
  where funInclude el = any (el =~) patterns
