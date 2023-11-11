module X12 where

import System.Environment (executablePath)

-- aliases
type Pos = (Int, Int)

type Transformation = Pos -> Pos

origin :: Pos
origin = (0, 0)

left :: Transformation
left (x, y) = (x - 1, y)

-- generics
type Pair a = (a, a)

mult :: (Pair Int) -> Int
mult (m, n) = m * n

copy :: a -> Pair a
copy x = (x, x)

data Boolean = False | True

data Answer = Yes | No | Unknown

answers :: [Answer]
answers = [Yes, No, Unknown]

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown

addt :: (Num a) => (a, a) -> a
addt (x, y) = x + y

-- make tuple -> curry
addc :: (Num a) => a -> a -> a
addc = curry addt

-- make curry -> tuple
adc2 :: (Num a) => (a, a) -> a
adc2 = uncurry addc

--           ======  ...    ====  ...   ...
data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

-- data constructor actually are functions:
-- Circle :: Float -> Shape
-- Rect :: Float -> Float -> Shape

data Maybe1 a = Nothing1 | Just1 a

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv a b = Just (a `div` b)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- recursive data type
data Nat = Zero | Succ Nat

zero :: Nat
zero = Zero

one :: Nat
one = Succ Zero

two :: Nat
two = Succ one

three :: Nat
three = Succ two

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add1 :: Nat -> Nat -> Nat
add1 a b = int2nat (nat2int a + nat2int b)

add2 :: Nat -> Nat -> Nat
add2 Zero n = n
add2 (Succ m) n = Succ (add2 m n)

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul (Succ x) y = add2 y (mul x y)

-- simple expression: 1 + 2 * 3
data Expr = Val Int | Add Expr Expr | Mul Expr Expr

data Tree a = Leaf a | Node (Tree a) (Tree a)

f11 :: Expr
f11 = Add (Val 1) (Mul (Val 2) (Val 3))

size :: Expr -> Int
size (Val _) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

--          Val                  Add                    Mul
folde :: (Int -> Int) -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> Expr -> Int
folde f0 f1 f2 (Add x y) = f1 xv yv
  where
    xv = folde f0 f1 f2 x
    yv = folde f0 f1 f2 y
folde f0 f1 f2 (Mul x y) = f2 xv yv
  where
    xv = folde f0 f1 f2 x
    yv = folde f0 f1 f2 y
folde f0 _ _ (Val x) = f0 x

evale :: Expr -> Int
evale = folde id (+) (*)
