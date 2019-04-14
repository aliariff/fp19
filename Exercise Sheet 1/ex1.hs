module S1Ex1 where

-- | undInt is function to use "and" between two boolean value and respresent the result as integer, 1 for True and 0 for False
--
-- Examples:
--
-- >>> andInt True True
-- 1
--
-- >>> andInt True False
-- 0
--
-- >>> andInt False True
-- 0
--
-- >>> andInt False False
-- 0
andInt :: Bool -> Bool -> Int
andInt True True = 1
andInt _    _    = 0

-- | andList is function to merge to list of Int and list of Bool and operate "and" logic to each of them. We perceive non zero Int is True value and 0 is False. If the length of both lists is different the function will return 0. Ex: andList [1,2,3] [True,True,True] = 1 and True and 2 and True and 3 and True = True. andList [-1,0,1] [True,True,True] = -1 and True and 0 and True and 1 and True = 0
--
-- Examples:
--
-- >>> andList [-1,0,1] [True,True,True]
-- 0
--
-- >>> andList [-1,2,3] [True,True,True]
-- 1
--
-- >>> andList [1,2] [True,True,True]
-- 0
--
-- >>> andList [] []
-- 1
andList :: [Int] -> [Bool] -> Int
andList [] [] = 1
andList [] _  = 0
andList _  [] = 0
andList (x : xs) (y : ys) | x == 0    = 0
                          | not y     = 0
                          | otherwise = andList xs ys

-- | boolToInt is function to convert list of boolean to integer with the conversion rule is custom defined
--
-- Examples:
--
-- >>> boolToInt [True,False,True] (\x -> if x == True then 1 else 0)
-- [1,0,1]
--
-- >>> boolToInt [False] (\x -> if x == True then 1 else 0)
-- [0]
--
-- >>> boolToInt [True] (\x -> if x == True then 1 else 0)
-- [1]
--
-- >>> boolToInt [] (\x -> if x == True then 1 else 0)
-- []
boolToInt :: [Bool] -> (Bool -> Int) -> [Int]
boolToInt []       _ = []
boolToInt (x : xs) f = f x : boolToInt xs f
