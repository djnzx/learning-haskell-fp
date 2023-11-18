module Y08Monads2 where

class Applicative m => Monad1 m
  where
    (>>=) :: m a -> (a -> m b) -> m b
    return1 :: a -> m a -- pure
    return1 = pure

instance Monad1 Maybe
  where
    Nothing >>= f = Nothing
    Just x >>= f = f x

instance Monad1 []
  where
    xs >>= f = concat (map f xs)
    --         [y | x <- xs, y <- f x]

f1 :: [[Int]]
f1 = fmap (\x -> [-x, x]) [1,2,3]

-- work for any monadic wrapper, not only for list
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do
                x <- xs
                y <- ys
                return (x, y)

-- also called cartesian product, this is tied to list
pairs2 :: [a] -> [b] -> [(a,b)]
pairs2 xs ys = [(x, y) | x <- xs, y <- ys]

-- State monad
-- ST a -> State -> (a, State)

quadraticRoots1 :: Double -> Double -> Double -> (Double, Double)
quadraticRoots1 a b c = (x1, x2)
  where
    d = b^2 - 4*a*c
    x1 = (-b + sqrt d) / (2*a)
    x2 = (-b - sqrt d) / (2*a)

quadraticRoots2 :: Double -> Double -> Double -> (Double, Double)
quadraticRoots2 a b c = 
  let
    d = b^2 - 4*a*c
    x1 = (-b + sqrt d) / (2*a)
    x2 = (-b - sqrt d) / (2*a)
  in (x1, x2)
