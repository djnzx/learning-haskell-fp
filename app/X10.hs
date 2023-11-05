module X10 where

-- high order function

twice :: (a -> a) -> a -> a
twice f x = f (f x)

f1 :: [Integer]
f1 = map (+ 1) [1, 2, 3, 4, 5]

map1 :: (a -> b) -> [a] -> [b]
map1 f xs = [f x | x <- xs]

map2 :: (a -> b) -> [a] -> [b]
map2 _ [] = []
map2 f (x : xs) = f x : map f xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 f xs = [x | x <- xs, f x]

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 f (x : xs)
  | f x = x : filter2 f xs
  | otherwise = filter2 f xs

sum1 :: [Int] -> Int
sum1 = foldr (+) 0

product1 :: [Int] -> Int
product1 = foldr (*) 1

or1 :: [Bool] -> Bool
or1 = foldr (||) False

and1 :: [Bool] -> Bool
and1 = foldr (&&) True

len1 :: [a] -> Int
len1 [] = 0
len1 (_ : xs) = 1 + len1 xs

rev1 :: [a] -> [a]
rev1 [] = []
rev1 (x : xs) = rev1 xs ++ [x]

rev2 :: [a] -> [a]
rev2 = foldr (\x xs -> xs ++ [x]) []

-- starts from the last + acc
-- ands with first + acc
--
-- foldr2 (:) "-"    ['a','b','c']
-- foldr2 (:) "c-"   ['a','b']
-- foldr2 (:) "bc-"  ['a']
-- foldr2 (:) "abc-" []
-- "abc-"
foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 _ v [] = v
foldr2 f v (x : xs) = f x (foldr2 f v xs)

-- TODO: fusion properties
-- TODO: babana split rule

compose1 :: (b -> c) -> (a -> b) -> a -> c
compose1 f g = \x -> f (g x)

compose2 :: (b -> c) -> (a -> b) -> a -> c
compose2 f g x = f (g x)

andThen2 :: (a -> b) -> (b -> c) -> a -> c
andThen2 f g x = g (f x)

all1 :: (a -> Bool) -> [a] -> Bool
all1 p xs = and [p x | x <- xs]

any1 :: (a -> Bool) -> [a] -> Bool
any1 p xs = or [p x | x <- xs]

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 p (x : xs)
  | p x = x : takeWhile1 p xs
  | otherwise = []

dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 p (x : xs)
  | p x = dropWhile1 p xs
  | otherwise = x : xs

f2a :: (t -> a) -> (t -> Bool) -> [t] -> [a]
f2a f p xs = [f x | x <- xs, p x]

f2b :: (t -> a) -> (t -> Bool) -> [t] -> [a]
f2b f p xs = map f (filter p xs)