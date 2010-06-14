module Shaker.Regex
where

import Text.Regex.Posix
import Data.List

-- |Remove all elemts matching one of the given regexp
filterListWithRegexp :: [String] -> [String] -> [String]
filterListWithRegexp list ignore
  | elem "" list = filter funIgnore trimmedList
  | otherwise = filter funIgnore list
    where funIgnore el = not $ any (el =~) ignore
          trimmedList = delete "" list


