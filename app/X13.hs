module X13 where
import Text.Show.Functions
--import Data.List (subsequences)

-- countdown
-- having 1 3 7 10 25 50
-- using + - * /
-- combine them to make 765
-- constraints:
--   * intermediate results are natural: 1,2,3...
--      - no negative
--      - no zero
--      - no proper fraction
--   * all numbers used ot most once
-- example: (25 - 10) * (50 + 1) = 765
-- there are 780 solutions
-- if target = 831 -> no solutions
-- http://www.cs.nott.ac.uk/~pszgmh/
-- http://www.cs.nott.ac.uk/~pszgmh/pgp-countdown.hs  

-- operation definition
data Op = Add | Sub | Mul | Div
  deriving Show
  
-- operation implementation
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- with such improvements: valid 20x faster, whole app 16x times faster
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y -- True
valid Sub x y = x > y
valid Mul x y = x <= y && x /= 1 && y /= 1 -- True
valid Div x y = x `mod` y == 0 && y /= 1

--         value   operation application
data Expr = Val Int | App Op Expr Expr
  deriving Show

-- array due to the fact it can fail
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- idea 1
-- ["abc","bac","cba","bca","cab","acb"]
permutations :: [a] -> [[a]]
permutations xs0 = xs0 : perms xs0 []
  where
    perms [] _ = []
    perms (t : ts) is = foldr interleave (perms ts (t : is)) (permutations is)
      where
        interleave xs r = let (_, zs) = interleave' id xs r in zs
        interleave' _ [] r = (ts, r)
        interleave' f (y : ys) r =
          let (us, zs) = interleave' (f . (y :)) ys r
           in (y : us, f (t : y : us) : zs)

-- idea2
-- non-empty sets
-- [1,2,3] -> [[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]
choicesNe :: [a] -> [[a]]
choicesNe [] = []
choicesNe (x : xs) = [x] : foldr f [] (choicesNe xs)
  where
    f ys r = ys : (x : ys) : r

-- all possible ways to pick 0 or more elements
-- choices [1,2] -> [ [], [1], [2], [1,2], [2,1] ]
choices :: [a] -> [[a]]
choices xs = error "TODO" -- [] : choicesNe xs

-- choices [x] = [[x]]
-- choices xs = filter (not . null) $ subsequences xs
-- choices (x : xs) = [[]] ++ choices [x] ++ choices xs -- error "TODO" --  map (\a -> x : a) (choices xs)

-- all values from the expression
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

-- is expression a solution for the given list and target
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- brute force solution:
-- split into two non empty parts in all possible ways
-- split [1,2,3,4] -> [ ([1],[2,3,4]), ([1,2],[3,4]), ([1,2,3],[4]) ]
split :: [a] -> [([a], [a])]
split _ = error "TODO"

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

-- combine in all possible ways (apply all possible functions allowed)
combine :: Expr -> Expr -> [Expr]
combine l r = [App op l r | op <- [Add, Sub, Mul, Div]]

-- final solution (entry point)
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- better, combining expression and evaluation
type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App op l r, apply op x y) | op <- [Add, Sub, Mul, Div], valid op x y]

-- due to fusing and early return = X10 times faster!
solutions' :: [Int] -> Int -> [Expr]
solutions' ns target = [e | ns' <- choices ns, (e, m) <- results ns', m == target]

-- further optimizations:
-- x * y = y * x
-- x * 1 = x
-- x + y = y + x
-- x + 0 = x
