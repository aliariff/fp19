module S3Ex2 where

drop_mult :: Int -> [Int] -> [Int]
drop_mult x xs = [ y | y <- xs, y `mod` x /= 0 ]

dropall :: [Int] -> [Int]
dropall []       = []
dropall (x : xs) = x : dropall (drop_mult x xs)

primes :: [Int]
primes = dropall [2 ..]

-- |
-- >>> goldbach (-1)
-- []
-- >>> goldbach 1
-- []
-- >>> goldbach 2
-- []
-- >>> goldbach 3
-- []
-- >>> goldbach 4
-- []
-- >>> goldbach 5
-- []
-- >>> goldbach 6
-- [(3,3)]
-- >>> goldbach 50
-- [(3,47),(7,43),(13,37),(19,31)]
goldbach :: Int -> [(Int, Int)]
goldbach n =
  [ (x, y)
  | x <- takeWhile (< n) primes
  , odd x
  , y <- takeWhile (< n) primes
  , odd y
  , x <= y
  , x + y == n
  ]
