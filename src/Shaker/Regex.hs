-- | Allow to filter a list of string with fileListenInfoInclude and exclude patterns
module Shaker.Regex(
  processListWithRegexp
) where

import Text.Regex.Posix
import Data.List

-- | Filter all elements matching fileListenInfoInclude patterns and 
-- remove all elements matching exclude patterns to the result.
-- 
-- If no fileListenInfoInclude pattern is given, all elements are accepted minus those matching exclude patterns.
--
-- If no exclude pattern is given, all elements matching fileListenInfoInclude patterns are taken.
processListWithRegexp :: 
  [String] -- ^ Initial list to filter
  -> [String] -- ^ exclude patterns (regex)
  -> [String] -- ^ fileListenInfoInclude patterns (regex)
  -> [String] -- ^ List with all elements matching fileListenInfoInclude patterns minus all elements matching exclude patterns
processListWithRegexp list [] [] = list
processListWithRegexp list fileListenInfoIgnore [] = nub $ list \\ getExcluded list fileListenInfoIgnore 
processListWithRegexp list [] fileListenInfoInclude = nub $ getIncluded list fileListenInfoInclude
processListWithRegexp list fileListenInfoIgnore fileListenInfoInclude = 
  nub $ getIncluded list fileListenInfoInclude \\ getExcluded list fileListenInfoIgnore 

getExcluded :: [String] -> [String] -> [String]
getExcluded list patterns = filter funExclude list
 where funExclude el = any (\a -> el =~+ (a,compIgnoreCase, execBlank) ) patterns

getIncluded :: [String] -> [String] -> [String]
getIncluded list patterns = filter funInclude list
  where funInclude el = any (\a -> el =~+ (a,compIgnoreCase, execBlank) ) patterns

(=~+) :: ( RegexMaker regex compOpt execOpt source, RegexContext regex source1 target ) => source1 -> (source, compOpt, execOpt) -> target
source1 =~+ (source, compOpt, execOpt) = match (makeRegexOpts compOpt execOpt source) source1


