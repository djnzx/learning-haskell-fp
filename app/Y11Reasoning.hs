module Y11Reasoning where

data Expr a = Var a
            | Val Int
            | Add (Expr a) (Expr a)

-- x + 1

ex1 :: Expr Char
ex1 = Add (Var 'x') (Val 1)

ex2 :: Expr String
ex2 = Add (Var "x") (Val 1)

instance Functor Expr where
  fmap f (Var x)   = Var (f x)
  fmap f (Val n)   = Val n
  fmap f (Add a b) = Add (fmap f a) (fmap f b)

instance Applicative Expr where
  pure x   = Var x
  -- pure x   = Var x
  -- pure (Val n)   = error ""
  -- pure (Add a b) = error ""
  x <*> y = error "think"

instance Monad Expr where
  return x = Var x
  (Var v)     >>= f = f v
  (Val n)     >>= f = Val n -- it's a value. function can't be applied
  (Add a1 a2) >>= f = Add (a1 >>= f) (a2 >>= f)

-- substitution
f :: Char -> Expr a
f 'x' = Add (Val 1) (Val 2)
f 'y' = Val 3

v :: Expr Char
v = Add (Var 'x') (Var 'y')

-- since flatMap is defined we can write
w :: Expr b
w = v >>= f

reasoning1 :: (Eq a, Num a) => a -> a -> Bool
reasoning1 x y = x + y == y + x

reasoning2 :: (Eq a, Num a) => a -> a -> Bool
reasoning2 x y = x * y == y * x

reasoning3 :: (Eq a, Num a) => a -> a -> a -> Bool
reasoning3 x y z = (x + y) + z == x + (y + z)

reasoning4 :: (Eq a, Num a) => a -> a -> a -> Bool
reasoning4 x y z = (x + y) * z == x * y + x * z

reasoning5 :: (Eq a, Num a) => a -> a -> a -> Bool
reasoning5 x y z = z * (x + y) == z * x + z * y

reasoning6 :: (Num a, Eq a) => a -> a -> a -> Bool
reasoning6 x a b = (x + a) * (x + b) == x ^ 2 + x * (a + b) + a  * b

-- basic
isZero :: Int -> Bool
isZero n = n == 0

-- the second relies on the first
isZero1 :: Int -> Bool
isZero1 0 = True
isZero1 n = False

-- disjoint, non-overlapped patterns
isZero2 :: Int -> Bool
isZero2 0 = True
isZero2 n | n/= 0 = False
