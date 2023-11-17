module Y02 where

-- sudoku.org.uk
-- http://www.cs.nott.ac.uk/~pszgmh/sudoku.lhs

import Data.List ( (\\), transpose )
import Y02Types ( Choices, Grid, Matrix, Row, Value )
import Y02Data

values :: [Value]
values =  ['1'..'9']

is_empty :: Value -> Bool
is_empty = (== '.')

is_single :: [a] -> Bool
is_single [_] =  True
is_single _   =  False

-- extracting rows: (rows . rows == id)
rows :: Matrix a -> [Row a]
rows = id

-- extracting cols: (cols . cols == id)
cols :: Matrix a -> [Row a]
cols = transpose

-- extracting boxes: (boxes . boxes == id), (concat . split = id)
boxes :: Matrix a -> [Row a]
boxes =  unpack . map cols . pack
           where
             pack   = split . map split
             split  = chop boxsize
             unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop _ [] =  []
chop n xs =  take n xs : chop n (drop n xs)

-- check filled matrix
-- all = forall(p)(xs)
-- all p xs - and [p x | x <- xs]
is_valid :: Grid -> Bool
is_valid g = all nodups (rows g) &&
             all nodups (cols g) &&
             all nodups (boxes g)

-- TODO: what about `.` or we are about to check only filled matrices
nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = not (elem x xs) && nodups xs

-- given cell -> return all possible values for this cell
choice :: Char -> [Char]
choice c | is_empty c = values
         | otherwise = [c]

-- fill it in all possible ways
-- brutefoce w/o any logic
choices :: Grid -> Matrix Choices
choices = map (map choice)

-- cartesian product
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [y : ys | y <- xs, ys <- cp xss]

explode :: Matrix [a] -> [Matrix a]
explode m = cp (map cp m)

-- basic solver, 9^51, never terminates
solve1 :: Grid -> [Grid]
solve1 = filter is_valid . explode . choices

-- exlude from first list values from the second list
minus :: (Eq a) => [a] -> [a] -> [a]
minus [x] _ = [x]
minus xs ys = xs \\ ys

-- filter subarrays with length 1
-- ["1234","1","34","3"] => "13"
singles :: [[a]] -> [[a]]
singles = filter is_single

-- eliminate impossible cases
-- ["1234","1","34","3"]
--   x x        x
-- ["24",  "1", "4","3"]
reduce :: Row Choices -> Row Choices
reduce xss = [xs `minus` sngls | xs <- xss]
               where
                 sngls = concat (singles xss)

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxes . pruneBy cols . pruneBy rows
        where 
          pruneBy f = f . map reduce . f

-- basic solver, 9^24, never terminates
solve2 :: Grid -> [Grid]
solve2 = filter is_valid . explode . prune . choices

-- iterate prune until result is the same
fix :: (Eq a) => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
          where x' = f x

-- better, but gentle will take 10^29, never terminates
solve3 :: Grid -> [Grid]
solve3 = filter is_valid . explode . fix prune . choices

-- VERSION 4 ---------------------------------------------
-- matrix is void if no choices for a cell for some reason (contains empty Choices in any place)
is_void :: Matrix Choices -> Bool
is_void m = any (any null) m

consistent :: Row Choices -> Bool
consistent xs = nodups (singles xs)

-- safe -- is consistent - has any duplicates of single values in any direction
is_safe :: Matrix Choices -> Bool
is_safe m = all consistent (rows m) && 
            all consistent (cols m) && 
            all consistent (boxes m)

blocked :: Matrix Choices -> Bool
blocked m = is_void m || not (is_safe m)

is_complete :: Matrix Choices -> Bool
is_complete  =  all (all is_single)

collapse :: Matrix [a] -> [Matrix a]
collapse =  cp . map cp

-- expand only 1st cell has more than one choice ["1234", "56", "78"] 
-- => [["1", "56", "78"], 
--     ["2", "56", "78"], 
--     ["3", "56", "78"], 
--     ["4", "56", "78"]]
expand1 :: Matrix Choices -> [Matrix Choices]
expand1 m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
              where
                has_more1 = not . is_single
                (rows1, row : rows2) = break (any has_more1) m
                (row1, cs : row2)    = break has_more1 row

search :: Matrix Choices -> [Grid]
search m | blocked m = []
         | is_complete m = collapse m
         | otherwise = [ g | m' <- expand1 m, g <- search (prune m')]

-- filter before explode
solve4 :: Grid -> [Grid]
solve4 = search . prune . choices
--                        Grid -> Matrix Choices
--                Matrix Choices -> Matrix Choices
--       Matrix Choices -> [Grid]

main :: IO ()
main =  putStrLn (unlines (head (solve4 easy)))