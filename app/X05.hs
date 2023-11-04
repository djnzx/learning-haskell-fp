module X05 where

-- expr :: type
-- types
-- type Bool = True | False
-- Bool, Char, String, Int,
-- Float, Double
-- List = [...]
-- empty List = []

f1 :: Bool
f1 = True

f2 :: Bool
f2 = not False

f3 :: [Bool]
f3 = [True, True]

f4 :: [Char]
f4 = ['a', 'b']

f5 :: [String]
f5 = ["a", "b"]

-- List can be nested
f6 :: [[Char]]
f6 = [['a'], ['a', 'b']]

-- empty list has no type, can be any type
f7 :: [a]
f7 = []

-- tuples: pair, triple, ...
f8 :: (Integer, Double, String, Char, Bool)
f8 = (1, 2.5, "Jib", 'a', True)

f9 :: Bool -> Bool
f9 x = not x

-- we can eliminate parameter declaration if they are 1to1 mapped
f9a :: Bool -> Bool
f9a = not

-- must be passed at a time
add1 :: (Num a) => (a, a) -> a
add1 (x, y) = x + y

-- currying
add2 :: (Num a) => a -> a -> a
add2 x y = x + y

-- making currying from tupled
add3 :: (Num a) => a -> a -> a
add3 x y = add1 (x, y)

-- more than one type constraint
zeroto :: (Num a, Enum a) => a -> [a]
zeroto n = [0 .. n]

mult1 :: (Num a) => a -> a -> a -> a -> a
mult1 a b c d = a * b * c * d

-- right associative
mult2 :: (Num a) => a -> (a -> (a -> (a -> a)))
mult2 a b c d = a * b * c * d

-- (+) :: Num a => a -> a -> a

second :: [a] -> a
second xs = head (tail xs)

swap :: (b, a) -> (a, b)
swap (x, y) = (y, x)