module S1Ex4 where

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
(^^^) (x : xs) (y : ys) | y > 0     = x ^ y * xs ^^^ ys
                        | otherwise = xs ^^^ ys
