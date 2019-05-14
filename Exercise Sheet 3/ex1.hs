module S3Ex1 where

nextCollatz :: Int -> Int
nextCollatz n | n < 1          = error "Cannot have negative number"
              | n `mod` 2 == 0 = n `div` 2
              | otherwise      = 3 * n + 1

collatz :: Int -> [Int]
collatz n = n : collatz (nextCollatz n)

-- |
-- >>> total_stopping_time 1
-- 0
-- >>> total_stopping_time 2
-- 1
-- >>> total_stopping_time 3
-- 7
-- >>> total_stopping_time 0
-- 0
-- >>> total_stopping_time (-1)
-- 0
total_stopping_time :: Int -> Int
total_stopping_time n = length (takeWhile (> 1) (collatz n))

-- |
-- >>> check_collatz 1
-- True
-- >>> check_collatz 2
-- True
-- >>> check_collatz 3
-- True
-- >>> check_collatz 4
-- True
-- >>> check_collatz 5
-- True
-- >>> check_collatz 0
-- False
-- >>> check_collatz (-1)
-- False
check_collatz :: Int -> Bool
check_collatz n | n == 1                      = True
                | n < 1                       = False
                | (total_stopping_time n) > 0 = check_collatz (n - 1)
                | otherwise                   = False
