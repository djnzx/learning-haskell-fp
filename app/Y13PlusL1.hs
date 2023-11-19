module Y13PlusL1 where

appnd :: [a] -> [a] -> [a]
-- appnd = (++)
appnd [] ys = ys
appnd (x:xs) ys = x : (xs ++ ys)

rev :: [a] -> [a]
-- rev = reverse
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- sum [1..n] = n * (n+1) / 2

-- reverse takes (n^2 + 3n + 2) / 2
-- because of rev defined in terms of ++

-- rev' xs ys = reverse xs ++ ys
-- this a definition we need to implement
rev' :: [a] -> [a] -> [a]
rev' [] ys     = ys             -- base case
rev' (x:xs) ys = rev' xs (x:ys) -- inductive

-- reverse without `++` is significantly better

-- tail recursive: complexity: n +2
rev1 :: [a] -> [a]
rev1 xs = rev1a xs []
          where
            rev1a [] ys = ys
            rev1a (x':xs') ys = rev1a xs' (x':ys)
