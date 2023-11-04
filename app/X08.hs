module X08 where

-- https://willamette.edu/~fruehr/haskell/evolution.html?

-- plain
fac1 :: Int -> Int
fac1 n = product [1 .. n]

fac2 :: Int -> Int
fac2 n = if n == 0 then 1 else n * fac2 (n - 1)

fac3 :: Int -> Int
fac3 = (\n -> (if ((==) n 0) then 1 else ((*) n (fac3 ((-) n 1)))))

-- recursive
fac4 :: Integer -> Integer
fac4 0 = 1
fac4 n = n * fac4 (n - 1)

-- tail recursive
fac5 :: Integer -> Integer
fac5 n = f5 n 1
  where
    f5 1 a = a
    f5 n a = f5 (n - 1) (a * n)

product1 :: (Num a) => [a] -> a
product1 [] = 1
product1 (x : xs) = x * product1 xs

len1 :: (Num a) => [a] -> a
len1 [] = 0
len1 (_ : xs) = 1 + len1 xs

rev1 :: [a] -> [a]
rev1 [] = []
rev1 (x : xs) = rev1 xs ++ [x]

zip1 :: [a] -> [b] -> [(a, b)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (x : xs) (y : ys) = (x, y) : zip1 xs ys

drop1 :: Int -> [a] -> [a]
drop1 0 xs = xs
drop1 _ [] = []
drop1 n (_ : xs) = drop1 (n - 1) xs

qs :: (Ord a) => [a] -> [a]
qs [] = []
qs [x] = [x]
qs (x : xs) = qs smaller ++ [x] ++ qs larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]
