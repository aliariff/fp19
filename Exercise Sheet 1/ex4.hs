module S1Ex4 where

-- | pow is helper function to implement ^
--
-- Examples:
--
-- >>> 0 `pow` 0
-- 1
--
-- >>> 2 `pow` 3
-- 8
--
-- >>> 4 `pow` 2
-- 16
pow :: Int -> Int -> Int
pow _ 0 = 1
pow x y = x * pow x (y - 1)

-- | evaluates to xs to the power of ys interpreted as vectors, where the negative entries of ys are ignored
--
-- Examples:
--
-- >>> [1, 4, 5] ^^^ [7, 2, 3]
-- 2000
--
-- >>> [1, 4, 5] ^^^ [5, -1, 0]
-- 1
--
-- >>> [1, 4, 5] ^^^ [5, -1, 0] * 10
-- 10
--
-- >>> 1 + [1, 4, 5] ^^^ [5, -1, 0] * 3
-- 4
--
-- >>> [4, 5] ^^^ [5, -1, 0]
-- 0
(^^^) :: [Int] -> [Int] -> Int
(^^^) [] [] = 1
(^^^) [] _  = 0
(^^^) _  [] = 0
(^^^) (x : xs) (y : ys) | y > 0     = x `pow` y * xs ^^^ ys
                        | otherwise = xs ^^^ ys
