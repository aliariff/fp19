module S2Ex1 where

data PriorityQueue a = EmptyQueue | Push a Int (PriorityQueue a)

-- For debugging purposes
instance Show a => Show(PriorityQueue a) where
  show EmptyQueue    = "[]"
  show (Push m n xs) = show m ++ "|" ++ show n ++ " : " ++ show xs

-- |
-- >>> p
-- 11|1 : 5|3 : 5|0 : 9|-1 : 7|3 : 8|-3 : []
p :: PriorityQueue Int
p = Push
  11
  1
  (Push 5 3 (Push 5 0 (Push 9 (-1) (Push 7 3 (Push 8 (-3) EmptyQueue)))))

-- |
-- >>> isWaiting 5 p
-- True
-- >>> isWaiting 6 p
-- False
isWaiting :: Eq a => a -> PriorityQueue a -> Bool
isWaiting _ EmptyQueue = False
isWaiting x (Push y _ ys) | x == y    = True
                          | otherwise = isWaiting x ys

-- |
-- >>> fromList [(11,1), (5,3), (5,0), (9,-1), (7,3), (8,-3)]
-- 11|1 : 5|3 : 5|0 : 9|-1 : 7|3 : 8|-3 : []
-- >>> fromList [('x',1), ('y',2)]
-- 'x'|1 : 'y'|2 : []
fromList :: [(a, Int)] -> PriorityQueue a
fromList []            = EmptyQueue
fromList ((x, y) : xs) = Push x y (fromList xs)

maxi :: Int -> Int -> Int
maxi a b | a > b     = a
         | otherwise = b

-- |
-- >>> maxPriority p
-- 3
maxPriority :: PriorityQueue a -> Int
maxPriority EmptyQueue    = minBound :: Int
maxPriority (Push _ n xs) = maxi n (maxPriority xs)

-- |
-- >>> remove (maxPriority p) p
-- 11|1 : 5|0 : 9|-1 : 7|3 : 8|-3 : []
remove :: Int -> PriorityQueue a -> PriorityQueue a
remove _ EmptyQueue = EmptyQueue
remove x (Push m n xs) | x == n    = xs
                       | otherwise = Push m n (remove x xs)

-- |
-- >>> value (maxPriority p) p
-- 5
value :: Int -> PriorityQueue a -> a
value x (Push m n xs) | x == n    = m
                      | otherwise = value x xs

-- |
-- >>> pop p
-- (5,11|1 : 5|0 : 9|-1 : 7|3 : 8|-3 : [])
-- >>> pop (Push 1 2 EmptyQueue)
-- (1,[])
pop :: PriorityQueue a -> (a, PriorityQueue a)
pop pq = (value maxP pq, remove maxP pq) where maxP = maxPriority pq

-- |
-- >>> toList p
-- [5,7,11,5,9,8]
toList :: PriorityQueue a -> [a]
toList EmptyQueue = []
toList pq         = x : toList xs where (x, xs) = pop pq
