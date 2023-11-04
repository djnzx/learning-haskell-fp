module X03 where

-- curried
add :: Int -> Int -> Int
add x y = x + y

add2 :: Int -> Int -> Int
-- infix notation
add2 x y = x `add` y

sum1 :: Integer
sum1 = sum [1 .. 10]

-- constant declaration
val :: Integer
val = 5

-- Ord - typeclass for comparison
qs :: (Ord a) => [a] -> [a]
qs [] = []
qs [x] = [x]
qs (x : xs) = qs ys ++ [x] ++ qs zs
  where
    ys = [a | a <- xs, a <= x] -- generators
    zs = [b | b <- xs, b > x]
