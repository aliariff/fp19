module S1Ex3 where

-- | myrem x y is the remainder of the integer division when dividing x by y
--
-- Examples:
--
-- >>> myrem 14 3
-- 2
--
-- >>> myrem 14 0
-- 14
--
-- >>> myrem 15 (-5)
-- 0
--
-- >>> myrem (-14) (-3)
-- 1
myrem :: Int -> Int -> Int
myrem x 0 = x
myrem x y | y < 0     = myrem x (-y)
          | x < 0     = myrem (x + y) y
          | x >= y    = myrem (x - y) y
          | otherwise = x

-- | given a list xs and an element x returns the number of occurences of x in xs
--
-- Examples:
--
-- >>> count 2 [0,2,2,0,2,5,0,2]
-- 4
--
-- >>> count (-7) [0,2,2,0,2,5,0,2]
-- 0
count :: Int -> [Int] -> Int
count _ [] = 0
count x (y : ys) | x == y    = 1 + count x ys
                 | otherwise = count x ys

-- | removeItem is helper function to remove all of a item occurences in a list
--
-- Examples:
--
-- >>> removeItem 2 [0,2,2,0,2,5,0,2]
-- [0,0,5,0]
--
-- >>> removeItem (-7) [0,2,2,0,2,5,0,2]
-- [0,2,2,0,2,5,0,2]
--
-- >>> removeItem 10 []
-- []
removeItem :: Int -> [Int] -> [Int]
removeItem _ [] = []
removeItem x (y : ys) | x == y    = removeItem x ys
                      | otherwise = y : removeItem x ys

-- | given a list xs returns a list of pairs contains the pair (x,n) if and only if x occurs in xs n times and n > 0
--
-- Examples:
--
-- >>> simplify [0,2,2,0,2,5,0,2]
-- [(0,3),(2,4),(5,1)]
--
-- >>> simplify []
-- []
simplify :: [Int] -> [(Int, Int)]
simplify []       = []
simplify (x : xs) = (x, count x (x : xs)) : simplify (removeItem x xs)

-- | given two lists of pairs xs and ys concatenates these lists where each "multiple occurence" is simplified as follows: If xs contains a pair (x,n) and ys contains (x,m), then the result contains (x,n+m)
--
-- Examples:
--
-- >>> multUnion [(0,3),(2,4),(5,1)] [(-1,1),(0,4)]
-- [(-1,1),(0,7),(2,4),(5,1)]
--
-- >>> multUnion [(-1,1),(0,4)] [(0,3),(2,4),(5,1)]
-- [(-1,1),(0,7),(2,4),(5,1)]
--
-- >>> multUnion [] []
-- []
multUnion :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
multUnion x  [] = x
multUnion [] y  = y
multUnion ((x, n) : xs) ((y, m) : ys)
  | x == y    = (x, n + m) : multUnion xs ys
  | x > y     = (y, m) : multUnion ((x, n) : xs) ys
  | otherwise = (x, n) : multUnion xs ((y, m) : ys)
