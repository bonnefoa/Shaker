-- | Allow to filter a list of string with include and exclude patterns
module Shaker.Regex(
  processListWithRegexp
) where

import Text.Regex.Posix
import Data.List

-- | Filter all elements matching include patterns and 
-- remove all elements matching exclude patterns to the result.
-- 
-- If no include pattern is given, all elements are accepted minus those matching exclude patterns.
--
-- If no exclude pattern is given, all elements matching include patterns are taken.
processListWithRegexp :: 
  [String] -- ^ Initial list to filter
  -> [String] -- ^ include patterns (regex)
  -> [String] -- ^ exclude patterns (regex)
  -> [String] -- ^ List with all elements matching include patterns minus all elements matching exclude patterns
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
