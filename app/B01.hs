module B01 where

add7 :: (Num a) => a -> a -> a
add7 x y = x + y

seq1toN :: (Num a, Enum a) => a -> [a]
seq1toN n = [1 .. n]

sum1 :: Integer
sum1 = sum [1 .. 10]

qs :: (Eq a, Ord a) => [a] -> [a]
qs [] = []
qs [x] = [x]
qs (x : xs) = f(<=x) ++ [x] ++ f(>x)
  where
    f p = filter p xs 

f3 :: [Int]
f3 = filter even [1 .. 1000000]

-- sequence due to Applicative
seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (a:as) = do
                x <- a
                xs <- seqn as
                return (x:xs)
t= rem