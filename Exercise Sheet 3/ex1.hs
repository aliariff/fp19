module S3Ex1 where

nextCollatz :: Int -> Int
nextCollatz n | n `mod` 2 == 0 = n `div` 2
              | n `mod` 2 == 1 = 3 * n + 1

-- |
-- >>> collatz 1
-- [1,4,2,1]
-- >>> collatz 2
-- [2,1]
-- >>> collatz 3
-- [3,10,5,16,8,4,2,1]
-- >>> collatz 4
-- [4,2,1]
-- >>> collatz 5
-- [5,16,8,4,2,1]
-- >>> collatz 0
-- []
-- >>> collatz (-1)
-- []
collatz :: Int -> [Int]
collatz n
  | n > 0 = [n] ++ takeWhile (/= 1) (iterate nextCollatz (nextCollatz n)) ++ [1]
  | otherwise = []

-- |
-- >>> total_stopping_time 1
-- 3
-- >>> total_stopping_time 3
-- 7
-- >>> total_stopping_time 0
-- -1
-- >>> total_stopping_time (-1)
-- -1
total_stopping_time :: Int -> Int
total_stopping_time n = length (collatz n) - 1

-- |
-- >>> check_collatz 1
-- False
-- >>> check_collatz 2
-- True
-- >>> check_collatz 3
-- False
-- >>> check_collatz 4
-- True
-- >>> check_collatz 5
-- True
-- >>> check_collatz 0
-- False
-- >>> check_collatz (-1)
-- False
check_collatz :: Int -> Bool
check_collatz n | n > 0     = total_stopping_time n <= n
                | otherwise = False
