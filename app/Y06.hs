module Y06 where

-- applicative functors
-- abstraction over number of parameters

-- lift
-- pure :: a -> f a
-- pure = erorr "TODO"

-- function application
-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) = error "TODO"

-- applicative style
f1 :: Applicative f => (a1 -> a2 -> a3 -> b) -> f a1 -> f a2 -> f a3 -> f b
f1 g x y z = ((pure g <*> x) <*> y) <*> z

f2 :: (t1 -> t2 -> t3 -> t4) -> t1 -> t2 -> t3 -> t4
f2 g x y z = (((g x) y) z)

-- lifting to the context / pure
fmap0 :: (Applicative f) => a -> f a
fmap0 = pure

-- classic map / fmap
fmap1 :: (Applicative f) => (a -> b) -> f a -> f b
fmap1 g x = pure g <*> x
--  a->b fa f(a->b)    f a
--               fb    

-- first applicative example
fmap2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
fmap2 g a b = pure g <*> a <*> b

-- and so on
fmap3 :: (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 g a b c = pure g <*> a <*> b <*> c

-- applicative functor === applicative
-- Applicative[F[_]] extends Functor[F] { 
--   def pure[F](a: A): F[A]
--   def ap[A, B](f: F[A => B], fa: F[A]): F[B])
-- }
class Functor f => Applicative1 f
  where
    pure1 :: a -> f a
    (<**>) :: f (a -> b) -> f a -> f b -- ap

-- Maybe example
instance Applicative1 Maybe
  where
    pure1 x = Just x
    (<**>) Nothing fa = Nothing
    (<**>) (Just g) fa = fmap g fa

f3 :: Maybe Int
f3 = pure(+1) <*> (Just 1)

f4 :: Maybe Int
f4 = pure (+) <*> Just 1 <*> Just 2

f5 :: Maybe (Int -> Int)
f5 = pure (+) <*> Just 1

f6 :: Maybe (Int -> Int -> Int)
f6 = pure (+)

instance Applicative1 []
  where
    pure1 x = [x]
    gs <**> xs = [g x | g <- gs, x <- xs]

-- apply all functions to all elements
-- [2,3,4,11,12,13]
f7 :: [Int]
f7 = [(+1),(+10)] <*> [1,2,3]

-- [3,21,12,30]
f8 :: [Int]
f8 = pure (+) <*> [1,10] <*> [2,20]