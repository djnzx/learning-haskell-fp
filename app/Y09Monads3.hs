module Y09Monads3 where

-- state transformation idea: s -> (a, s)
newtype ST s a = S(s -> (a, s))
-- now ST is a data structure, capturing generic function inside, newtype = data without overhead

-- small function to remove type constructor
--     ------   ---
app :: ST s a -> s -> (a, s)
app (S f) s = f s

instance Functor (ST s) where
  -- fmap :: (a -> b) -> ST s a -> ST s b
  fmap fab stsa = S(\s -> let (a, s') = app stsa s
                              b       = fab a
                          in  (b, s') )

instance Applicative (ST s) where
  pure a = S(\s -> (a, s))
  -- (<*>) :: f (a -> b) -> f a -> f b
  stab <*> sta = S(\s -> let (fab, s') = app stab s
                             (a, s'')  = app sta s'
                             b         = fab a
                         in  (b, s'') )

instance Monad (ST s) where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S(\s -> let (x, s') = app st s
                     in app (f x) s' )

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

t1 :: Tree Char
t1 = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- replace with unique numbers: Tree a -> Tree Int

-- non-monadic impl: n -> n' -> n'' - easy to do mistake
rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n   = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
                      where
                        (l', n' ) = rlabel l n
                        (r', n'') = rlabel r n'

--          s    a 
fresh :: ST Int Int
--               a  state
fresh = S(\s -> (s, s + 1))

--                     s   a
mlabel :: Tree a -> ST Int (Tree Int)
mlabel (Leaf _) = do 
                    n <- fresh -- fresh is a state transformation which is using current value
                    return (Leaf n)
mlabel (Node l r) = do
                      l' <- mlabel l -- mlabel is a state transformation which is using value of tree and passing a state
                      r' <- mlabel r
                      return (Node l' r')

label :: Tree a -> Tree Int
label t = t'
  where
    (t', n') =  app (mlabel t) 0
