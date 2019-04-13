myrem :: Int -> Int -> Int
myrem x 0 = x
myrem x y | y < 0     = myrem x (-y)
          | x < 0     = myrem (x + y) y
          | x >= y    = myrem (x - y) y
          | otherwise = x

count :: Int -> [Int] -> Int
count x [] = 0
count x (y : ys) | x == y    = 1 + count x ys
                 | otherwise = count x ys

-- removeItem is helper function to remove all of a item occurences in a list
removeItem :: Int -> [Int] -> [Int]
removeItem x [] = []
removeItem x (y : ys) | x == y    = removeItem x ys
                      | otherwise = y : removeItem x ys

simplify :: [Int] -> [(Int, Int)]
simplify []       = []
simplify (x : xs) = (x, count x (x : xs)) : simplify (removeItem x xs)

multUnion :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
multUnion x  [] = x
multUnion [] y  = y
multUnion ((x, n) : xs) ((y, m) : ys)
  | x == y    = (x, n + m) : multUnion xs ys
  | x > y     = (y, m) : multUnion ((x, n) : xs) ys
  | otherwise = (x, n) : multUnion xs ((y, m) : ys)
