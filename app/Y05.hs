module Y05 where

inc :: [Int] -> [Int]
inc [] = []
inc (x:xs) = x+1 : inc xs

sqr :: [Int] -> [Int]
sqr [] = []
sqr (x:xs) = x^2 : inc xs

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : Y05.map f xs

inc2 :: [Int] -> [Int]
inc2 = Y05.map (+1)

-- increment for any F[_] which is a functor
inc3 :: Y05.Functor f => f Int -> f Int
inc3 = Y05.fmap (+1)

-- increment for any F[_] which is a Functor
--                     A  which is a Number
inc4 :: (Y05.Functor f, Num a) => f a -> f a
inc4 = Y05.fmap (+1)

sqr2 :: [Int] -> [Int]
sqr2 = Y05.map (^2)

-- functor!  F[_]
class Functor f 
  where
    fmap :: (a -> b) -> f a -> f b

-- [a] = [](a)
-- [] type constructor
-- [a] constructe type

instance Y05.Functor []
  where  
    -- fmap :: (a -> b) -> [a] -> [b]
    fmap = Y05.map

data Maybe a = Nothing | Just a
--   -----
--   type.c

instance Y05.Functor Y05.Maybe
  where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap g Y05.Nothing = Y05.Nothing
    fmap g (Y05.Just x) = Y05.Just (g x)

data Tree a = Leaf a 
            | Node (Tree a) (Tree a)
            -- deriving Show

t1 :: Tree Int
t1 = Node (Leaf 1) (Leaf 2)

t2 :: Tree Int
t2 = Leaf 1

instance Y05.Functor Tree
  where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (Y05.fmap g l) (Y05.fmap g r)

instance (Show a) => Show (Tree a) 
  where
    show (Leaf x) = "v:" ++ show x
    show (Node l r) = "(left: " ++ show l ++ ")(right: " ++ show r ++ ")"

t3 :: Tree Int
t3 = Y05.fmap (*10) t1

-- the idea of functor - do the same things with different content