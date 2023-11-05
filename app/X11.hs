module X11 where

-- What are recursive functions?
-- Why are they useful? Reasoning. Induction
-- Why are they difficult at first? Different way of thinking

-- Seven step process
-- 1. name: short descriptive!
-- 2! signature: input type and output type
-- 3! enumerate the cases: skeleton
-- 4. define simple (termination case)
-- 5. list the ingridients
-- 6. implement other cases
-- 7. think about the result
-- 8! generalize / simplify

sum1 :: (Num a) => [a] -> a
sum1 = foldr (+) 0

dropN :: Int -> [a] -> [a]
dropN _ [] = []
dropN 0 xs = xs
dropN n (_ : xs) = dropN (n - 1) xs

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x : xs) = x : dropLast xs

dropLast2 :: [a] -> [a]
dropLast2 [] = []
dropLast2 (x : xs)
  | null xs = []
  | otherwise = x : dropLast xs
