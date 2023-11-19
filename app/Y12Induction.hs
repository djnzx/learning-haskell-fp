module Y12Induction where

data Nat = Zero | Succ Nat

zero :: Nat
zero = Zero

one :: Nat
one = Succ zero

two :: Nat
two = Succ one

-- ...

inf :: Nat
inf = Succ inf

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)
