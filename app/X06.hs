module X06 where

-- conditionals
abs :: (Ord a, Num a) => a -> a
abs n = if n >= 0 then n else -n

-- guarded syntax
abs1 :: (Ord a, Num a) => a -> a
abs1 n
  | n >= 0 = n
  | otherwise = -n

signum :: (Ord a1, Num a1, Num a2) => a1 -> a2
signum n =
  if n < 0
    then -1
    else if n == 0 then 0 else 1

signum1 :: (Ord a1, Num a1, Num a2) => a1 -> a2
signum1 n
  -- cond = value
  | n < 0 = -1
  | n > 0 = 1
  | otherwise = 0

-- pattern matching, order matters
not5 :: Bool -> Bool
not5 False = True
not5 True = False

not1 :: Bool -> Bool
not1 x = if x then False else True

not2 :: Bool -> Bool
not2 x
  | x == True = False
  | x == False = True

not3 :: Bool -> Bool
not3 _
  | True = False
  | False = True

not4 :: Bool -> Bool
not4 _
  | True = False
  | otherwise = True

(&&&&) :: Bool -> Bool -> Bool
True &&&& True = True
_ &&&& _ = False

-- to make it lazy
(&&&) :: Bool -> Bool -> Bool
True &&& x = x
False &&& _ = False

-- explicit ordering, gight associative
f1 :: [Integer]
f1 = 1 : (2 : (3 : (4 : [])))

head :: [a] -> a
head (x : _) = x

-- partial. how to handle exceptions
tail2 :: [a] -> [a]
tail2 (_ : xs) = xs

-- lambda
f2a :: [Integer]
--          lambda         data
f2a = map (\x -> x + 10) [1, 2, 3]

f2b :: [Integer]
f2b = map (+ 10) [1, 2, 3]

add :: Int -> Int -> Int
add x y = x + y

-- that's how lambda connected with currying
add2 :: Int -> (Int -> Int)
add2 = \x -> (\y -> x + y)

odds1 :: (Num b, Enum b) => b -> [b]
odds1 n = map f [0 .. n - 1]
  where
    f x = x * 2 + 1

-- avoid naming used only once
odds2 :: (Num b, Enum b) => b -> [b]
odds2 n = map (\x -> x * 2 + 1) [0 .. n - 1]

add3 :: (Num a) => a -> a -> a
add3 x y = (+) x y

-- second parameter of a+b goes to result
add4 :: Integer -> Integer
add4 = (1 +)

-- first parameter of a+b goes to result
add5 :: Integer -> Integer
add5 = (+ 1)

next :: Integer -> Integer
next = (+ 1)

prev :: Integer -> Integer
prev = (-) 1

double :: Integer -> Integer
double = (* 2)

halve :: Double -> Double
halve = (/ 2)

isempty :: [a] -> Bool
isempty = null

-- patternmatch
safetail1 :: [a] -> [a]
safetail1 [] = []
safetail1 (_ : xs) = xs

-- conditional
safetail2 :: [a] -> [a]
safetail2 xs = if null xs then [] else tail xs

-- guard
safetail3 :: [a] -> [a]
safetail3 xs
  | not (null xs) = tail xs
  | otherwise = []

-- guarded 2
safetail4 :: [a] -> [a]
safetail4 xs
  | null xs = []
  | otherwise = tail xs

-- plain pattern matching
(|||) :: Bool -> Bool -> Bool
(|||) False False = False
(|||) _ _ = True

-- lazy pattern matching
(||||) :: Bool -> Bool -> Bool
(||||) True _ = True
(||||) False x = x

-- conditional
(|||||) :: Bool -> Bool -> Bool
(|||||) a b = if a || b then True else False

-- guarded
(||||||) :: Bool -> Bool -> Bool
(||||||) a b
  | not a && not b = False
  | otherwise = True
