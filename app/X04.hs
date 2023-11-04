module X04 where

f1 :: Integer
f1 = head [1, 2, 3, 4]

-- access by index
f2 :: Integer
f2 = [1, 2, 3, 4] !! 2

-- exception
f3 :: a
f3 = head []

f4 :: [Integer]
f4 = tail [1, 2, 3, 4]

f14 :: Integer
f14 = last [1, 2, 3, 4]

f15 :: [Integer]
f15 = init [1, 2, 3, 4]

f5 :: [Integer]
f5 = take 3 [1, 2, 3, 4]

f6 :: [Integer]
f6 = drop 1 [1, 2, 3]

f7 :: Integer
f7 = sum [1, 2, 3]

f8 :: Int
f8 = length [1, 2, 3]

f9 :: Int
f9 = product [1, 2, 3, 4]

f10 :: [Integer]
f10 = reverse [1, 2, 3]

f11 :: [Integer]
f11 = [1, 2, 3] ++ [4, 5, 6]

f12 :: [Integer]
f12 = 1 : [4, 5, 6]

-- Int vs Integer ???

-- function application has the highest order
-- f a + b    === f(a) + b , NOT f(a+b)
-- f(g(x))    === f (g x)
-- f(x, g(y)) === f x (g y)
-- f(x)g(y)   === f x * g y

double :: (Num a) => a -> a
double x = x + x

quad1 :: (Num a) => a -> a
quad1 x = double (double x)

quad2 :: (Num a) => a -> a
quad2 x = double $ double x

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1 .. n]

average :: (Foldable t) => t Int -> Int
average xs = sum xs `div` length xs

f13a :: Integer
f13a = b + c
  where
    b = 1
    c = 2

f13b :: Integer
f13b = b + c where b = 1; c = 2

-- it will fail on empty list
last1 :: [a] -> a
last1 [x] = x
last1 (_ : xs) = last1 xs

last2 :: [a] -> a
last2 xs = xs !! (length xs - 1)

last3 :: [a] -> a
last3 xs = head (reverse xs)

-- it will fail on empty list
init1 :: [a] -> [a]
init1 (_ : []) = []
init1 (x : xs) = x : init1 xs

init2 :: [a] -> [a]
init2 xs = take (length xs - 1) xs

init3 :: [a] -> [a]
init3 xs = reverse (tail (reverse xs))
