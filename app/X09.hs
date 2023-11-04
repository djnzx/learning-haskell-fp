module X09 where

and1 :: [Bool] -> Bool
and1 [] = True
and1 [x] = x
and1 (True : xs) = and1 xs
and1 (False : _) = False

and2 :: [Bool] -> Bool
and2 [] = True
and2 (True : xs) = and2 xs
and2 (False : _) = False

and3 :: [Bool] -> Bool
and3 [] = True
and3 (x : xs)
  | not x = False
  | otherwise = and3 xs

and4 :: [Bool] -> Bool
and4 [] = True
and4 (x : xs) = x && and4 xs

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x : xs) = x ++ concat1 xs

replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n x = x : replicate1 (n - 1) x

-- TODO: how to deal with exceptions
nth :: [a] -> Int -> a
nth (x : _) 0 = x
nth (_ : xs) n = nth xs (n - 1)

insert1 :: Int -> [Int] -> [Int]
insert1 a [] = [a]
insert1 a (x : xs)
  | a <= x = a : x : xs
  | otherwise = x : insert1 a xs

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x : xs) = insert1 x (insertionSort xs)

contains1 :: (Eq a) => a -> [a] -> Bool
contains1 _ [] = False
contains1 a (x : xs)
  | a == x = True
  | otherwise = contains1 a xs

-- merge two sorted lists into ONE sorted list
mergeSorted :: (Ord a) => [a] -> [a] -> [a]
mergeSorted xs [] = xs
mergeSorted [] ys = ys
mergeSorted (x : xs) (y : ys)
  | x < y = x : mergeSorted xs (y : ys)
  | otherwise = y : mergeSorted (x : xs) ys

indexOfMiddle :: [a] -> Int
indexOfMiddle xs = round (fromIntegral (length xs) / 2)

halve :: [a] -> ([a], [a])
halve xs = splitAt (indexOfMiddle xs) xs

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = mergeSorted (msort x) (msort y)
  where
    (x, y) = halve xs
