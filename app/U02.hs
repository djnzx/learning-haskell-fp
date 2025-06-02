module U02 where

-- quantified constraints
-- https://www.youtube.com/watch?v=d18Fdu6ayM8&list=PLD8gywOEY4HaG5VSrKVnHxCptlJv2GAn7&index=2

-- class MonadTrans t where
--   lift :: Monad m => m a -> t m a

-- newtype Stack t1 t2 m a  =Stack (t1 (t2 m) a)  

-- -- instance (Monad m , MonadTrans t)

-- instance (MonadTrans t1, MonadTrans t2) => MonadTrans (Stack t1 t2) where
--   lift = Stack . lift . lift

