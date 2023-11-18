module Y07Monads1 where

-- monads

data Expr = Val Int | Div Expr Expr

-- can fail with x/0
eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
-- safediv a b = Just (a `div` b)
-- safediv a b = fmap (`div` b) (Just a)
safediv a b = pure (`div` b) <*> (Just a)

safeeval1 :: Expr -> Maybe Int
safeeval1 (Val n) = Just n
safeeval1 (Div x y) = case safeeval1 x of
                       Nothing -> Nothing
                       Just n -> case safeeval1 y of
                                   Nothing -> Nothing
                                   Just m -> safediv n m

-- the problem
f1 :: Expr -> Expr -> Maybe (Maybe Int)
f1 x y = pure safediv <*> safeeval2 x <*> safeeval2 y

-- safeeval2 :: Expr -> Maybe Int
-- safeeval2 (Val n) = pure n
-- safeeval2 (Div x y) = pure safediv <*> safeeval2 x <*> safeeval2 y

-- into / then / bind / >>=
-- flatMap     *     *         **          **
(>>==) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>== f = case mx of
             Nothing -> Nothing
             Just x -> f x

safeeval2 :: Expr -> Maybe Int
safeeval2 (Val n) = pure n
safeeval2 (Div x y) = safeeval2 x >>== \n ->
                      safeeval2 y >>== \m ->
                      safediv n m

-- do notation. just a syntax sugar
safeeval3 :: Expr -> Maybe Int
safeeval3 (Val n) = pure n
safeeval3 (Div x y) = do
                        m <- safeeval3 x
                        n <- safeeval3 y
                        m `safediv` n
