module Shaker.Regex
where

import Text.Regex.Posix
import Data.List

filterListWithRegexp :: [String] -> [String] -> [String]
filterListWithRegexp list ignore
  | elem "" list = filter funIgnore trimmedList
  | otherwise = filter funIgnore list
    where funIgnore el = not $ any (el =~) ignore
          trimmedList = delete "" list


