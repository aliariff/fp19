module S2Ex3 where

unique :: Eq a => a -> [a] -> [a]
unique x y | filter (== x) y == [] = x : y
           | otherwise             = y

-- |
-- >>> removeDuplicates [1,1,2,1,-1,1,1,2,3,1]
-- [-1,2,3,1]
-- >>> removeDuplicates []
-- []
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldr unique []

len :: [a] -> Int
len = foldr (\_ n -> 1 + n) 0

-- |
-- >>> differentDigits 08052019
-- 6
-- >>> differentDigits 111231112111
-- 3
differentDigits :: Int -> Int
differentDigits x = len (removeDuplicates (show x))
