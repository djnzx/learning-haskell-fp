module U01 where

import Debug.Trace

-- https://www.youtube.com/watch?v=S_HSt6jEtWM&list=PLD8gywOEY4HaG5VSrKVnHxCptlJv2GAn7&index=1

unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr gen s =
  case gen s of
    Nothing -> []
    Just (a, s') -> a : unfoldr gen s'

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' lo hi =
  unfoldr
    (\ cur -> if cur > hi then Nothing else Just (cur, cur + 1))
    lo

fibs :: [Int]
fibs =
  unfoldr
  (\ (n1, n2) -> traceShow(n1,n2) $ Just (n1, (n2, n1 + n2)))    
  (0, 1)

map' :: (a -> b) -> [a] -> [b]
map' f xs =
  unfoldr
    (\ l ->
      case l of 
        [] -> Nothing
        y : ys -> Just( f y, ys)
    )
    xs

-- filter' :: (a -> Boolean) -> [a] -> [a]
-- filter' p xs =
--   unfoldr
--     (\ l ->
--       case l of [] -> Nothing
--       y : ys -> if p y then Just(y, ys)
--     )
--     xs

zip' :: [a] -> [b] -> [(a, b)]
zip' xs ys =
  unfoldr
  (\ s ->
    case s of
      (x:s, y:ys) -> Just((x, y), (xs, ys))
      _ -> Nothing 

  )
  (xs, ys)

-- think about 
--  data List a = Nil | Cons a (List a)
--  data ListF a r = NilF | ConsF a r