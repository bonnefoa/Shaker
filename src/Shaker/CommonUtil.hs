module Shaker.CommonUtil
  where

separateEqual :: Eq a => [a] -> [[a]]
separateEqual [] = []
separateEqual lst = foldl separateEqual' [] lst

separateEqual' :: Eq a => [[a]] -> a -> [[a]]
separateEqual' [] el = [[el]]
separateEqual' (fstLst:xs) el | el `elem` fstLst = fstLst : separateEqual' xs el
                              | otherwise =  (fstLst++[el]):xs
        
trimList :: [String] -> [String]
trimList = map (dropWhile (== ' '))
