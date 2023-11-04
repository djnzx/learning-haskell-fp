module X07 where

f1a :: [Integer]
f1a = map (^ 2) [1 .. 5]

f1b :: [Integer]
f1b = [x ^ 2 | x <- [1 .. 5]]

-- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
-- cartesian product
f2a :: [(Integer, Char)]
f2a = [(x, y) | x <- [1, 2, 3], y <- ['a', 'b']]

-- [(1,'a'),(2,'a'),(3,'a'),(1,'b'),(2,'b'),(3,'b')]
f2b :: [(Integer, Char)]
f2b = [(x, y) | y <- ['a', 'b'], x <- [1, 2, 3]]

-- dependent
f3 :: [(Integer, Integer)]
f3 = [(x, y) | x <- [1 .. 3], y <- [x .. 3]]

flatten :: [[a]] -> [a]
flatten xss = [x | xs <- xss, x <- xs]

-- filters/guards
f4 :: [Integer]
--                    filter function
f4 = [x | x <- [1 .. 10], even x]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

-- actually efficient due to a lazy evaluation
isprime :: Int -> Bool
isprime n = factors n == [1, n]

-- what about optimizaion?
primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], isprime x]

sieve :: (Integral a) => [a] -> [a]
sieve [] = []
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

--                infinite list
allprimes :: [Integer]
allprimes = sieve [2 ..]

allprimesupto :: Integer -> [Integer]
allprimesupto n = takeWhile (< n) allprimes

-- [(1,"a"),(2,"b"),(3,"c"),(4,"d")]
-- take shorter list
f5 :: [(Integer, String)]
f5 = zip [1, 2, 3, 4, 5] ["a", "b", "c", "d"]

-- adjacent pairs
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- and === forall
issorted :: (Ord a) => [a] -> Bool
issorted xs = and [x <= y | (x, y) <- pairs xs]

-- positions 0 [1,2,0,4,0,3,0,6,7,0]
-- [2,4,6,9]
positions :: (Num a1, Enum a1, Eq a2) => a2 -> [a2] -> [a1]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

-- strings are lists of chars
f6 :: Int
f6 = length "abcde"

count :: (Eq a) => a -> [a] -> Int
count x xs = length [x' | x' <- xs, x == x']

pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x * x + y * y == z * z]

isperfect x = sum (init (factors x)) == x

perfects n = [x | x <- [1 .. n], isperfect x]

scalarproduct0 :: (Num a) => [a] -> [a] -> a
scalarproduct0 xs ys = sum [xs !! i * ys !! i | i <- [0 .. length xs - 1]]

scalarproduct1 :: [Int] -> [Int] -> Int
scalarproduct1 xs ys = sum [x * y | (x, y) <- xs `zip` ys]

scalarproduct2 :: [Int] -> [Int] -> Int
scalarproduct2 xs ys = sum (map (\t -> fst t * snd t) (zip xs ys))

scalarproduct3 :: [Int] -> [Int] -> Int
scalarproduct3 xs ys = sum (zipWith (*) xs ys)
