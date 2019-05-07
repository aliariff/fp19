module S2Ex2 where

data List a = Nil | Cons a (List a) deriving Show

-- |
-- >>> Cons 1 Nil == Cons 1 Nil
-- True
-- >>> Cons 1 (Cons 2 Nil) == Cons 1 (Cons 3 Nil)
-- False
instance Eq a => Eq(List a) where
  Nil       == Nil       = True
  Cons x xs == Cons y ys = x == y && xs == ys
  _         == _         = False

class Eq a => Mono a where
  binOp :: a -> a -> a

  one :: a

  pow :: Word -> a -> a
  pow 0 _ = one
  pow n x = binOp x (pow (n-1) x)

-- |
-- >>> binOp (-3) 2
-- -6
instance Mono Integer where
  one = 1
  binOp x y = x * y

-- |
-- >>> binOp (Cons 'a' Nil) (Cons 'b' (Cons 'c' Nil))
-- Cons 'a' (Cons 'b' (Cons 'c' Nil))
-- >>> binOp (Cons 1 Nil) (Cons 2 (Cons 3 Nil))
-- Cons 1 (Cons 2 (Cons 3 Nil))
instance Eq a => Mono(List a) where
  one = Nil
  binOp x           Nil = x
  binOp Nil         y   = y
  binOp (Cons x xs) y   = Cons x (binOp xs y)

-- |
-- >>> multiply [(3,Cons 'a' Nil),(1,Cons 'b' Nil),(2,Cons 'c' (Cons 'd' Nil))]
-- Cons 'a' (Cons 'a' (Cons 'a' (Cons 'b' (Cons 'c' (Cons 'd' (Cons 'c' (Cons 'd' Nil)))))))
multiply :: Mono a => [(Word, a)] -> a
multiply []            = one
multiply ((n, x) : xs) = binOp (pow n x) (multiply xs)
