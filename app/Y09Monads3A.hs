module Y09Monads3A where

-- state transformation idea: s -> (a, s)
newtype ST s a = ST(s -> (a, s))
-- now ST is a data structure, capturing generic function inside, newtype = data without overhead

instance Functor (ST s) where
  -- fmap :: (a -> b) -> ST s a -> ST s b
  fmap fab (ST sf) = ST(\s -> let (a, s') = sf s
                                  b       = fab a
                              in  (b, s') )

instance Applicative (ST s) where
  pure a = ST(\s -> (a, s))
  -- (<*>) :: ST s (a -> b) -> ST s a -> ST s b
  ST sfab <*> ST sa = ST(\s -> let (fab, s') = sfab s
                                   (a, s'')  = sa s'
                                   b         = fab a
                               in  (b, s'') )

instance Monad (ST s) where
  -- (>>=) :: ST s a -> (a -> ST s b) -> ST s b
  ST sa >>= fastsb = ST(\s -> let (a, s')  = sa s
                                  ST sb    = fastsb a
                                  (b, s'') = sb s'
                              in  (b, s'') )

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

t1 :: Tree Char
t1 = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- replace with unique numbers: Tree a -> Tree Int
--          s    a 
next :: ST Int Int
--               a  state
next = ST(\s -> (s, s + 1))
-- state modification occures only on the leaf, in other cases it is just passed

--                      s     a
mlabel :: Tree a -> ST Int (Tree Int)
mlabel (Leaf _) = do 
                    n <- next -- next is a state transformation which is using current value
                    return (Leaf n)
mlabel (Node l r) = do
                      l' <- mlabel l -- mlabel is a state transformation which is using value of tree and passing a state
                      r' <- mlabel r
                      return (Node l' r')

label :: Tree a -> Tree Int
label t = t'
  where
    ST stcomb = mlabel t
    (t', next) = stcomb 0
