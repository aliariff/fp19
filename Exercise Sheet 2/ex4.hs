module S2Ex4 where

data Polynomial a = Coeff a Int (Polynomial a) | Null deriving Show

q :: Polynomial Int
q = Coeff 4 3 (Coeff 2 1 (Coeff 5 0 Null))

-- |
-- >>> foldPoly (\c n m -> c * 3^n + m) 0 q
-- 119
foldPoly :: (a -> Int -> b -> b) -> b -> Polynomial a -> b
foldPoly _ e Null           = e
foldPoly f e (Coeff x y xs) = f x y (foldPoly f e xs)
