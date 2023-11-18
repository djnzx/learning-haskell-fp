module Y09Monads3B where

import Control.Monad.State

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

t1 :: Tree Char
t1 = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

--             s   a
next :: State Int Int -- (s -> (a, s))
--                  a    s
next = state(\s -> (s, s + 1)) -- next element definition
  
--                         s      a      
mlabel :: Tree a -> State Int (Tree Int)
mlabel (Leaf _) = do 
                    n <- state(\s -> (s, s + 1))
                    return (Leaf n)
mlabel (Node l r) = do
                      l' <- mlabel l
                      r' <- mlabel r
                      return (Node l' r') -- a

label :: Tree a -> Tree Int
label t = t'
  where
    stcomb = mlabel t -- State Int (Tree Int)
    (t', s') = runState stcomb 1

--                  s   a
countLeaf :: State Int () -- (s -> (a, s))
--                       a     s
countLeaf = state(\s -> ((), s + 1))
  
--                         s      a      
countTree :: Tree a -> State Int ()
countTree (Leaf _) = countLeaf
countTree (Node l r) = do
                         countTree l
                         countTree r
                         return ()

runcount :: Tree a -> Int
runcount t = s'
  where
    stcomb = mlabel t
    (t', s') = runState stcomb 0
