module Y10Monads4 where

import Data.Char

-- basic definition for arrays
-- it takes f: a-> b
map1 :: (a -> b) -> [a] -> [b]
map1 f [] = []
map1 f (x:xs) = f x : map1 f xs

-- definition generalized for any monadic function
-- it takes f: a -> m b
mapM1 :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM1 f [] = return []
mapM1 f (x:xs) = do y  <- f x
                    ys <- mapM1 f xs
                    return (y:ys)

-- map2 :: (Applicative m) => (a -> b) -> m a -> m b
-- map2 = fmap

conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing

-- flatten / join / generalized concat for any monad
join1 :: (Monad m) => m (m a) -> m a
join1 mmx = do mx <- mmx
               x <- mx
               return x

-- monadic laws / properties
-- 1
prop1 :: (Eq (m b), Monad m) => a -> (a -> m b) -> Bool
prop1 x f = (return x >>= f) == (f x)

-- 2. identity for return in context flatMap / bind: 1 * n = n, n * 1 = n
prop2 :: (Eq (m a), Monad m) => m a -> Bool
prop2 ma = (ma >>= return) == ma

-- 3. associativity                    ---- f ---    --- g ----
prop3 :: (Eq (m c), Monad m) => m a -> (a -> m b) -> (b -> m c) -> Bool
prop3 ma f g = mc == mz
               where
                 mb = ma >>= f          -- m b
                 mc = mb >>= g          -- m c
                 fg = \x -> (f x >>= g) -- a -> mc
                 mz = ma >>= fg         -- m c
