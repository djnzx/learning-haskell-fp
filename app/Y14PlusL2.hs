module Y14PlusL2 where

-- flatten tree

data Tree = Leaf Int | Node Tree Tree

-- simple, natural definition, but complexity is bad
flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l r) = flatten l ++ flatten r

-- constructive induction
flatten' :: Tree -> [Int] -> [Int]
-- flatten' t ns = flatten t ++ ns
flatten' (Leaf n) ns = n:ns
-- flatten' (Node l r) ns = (flatten l ++ flatten r) ++ ns
-- flatten' (Node l r) ns = flatten l ++ (flatten r ++ ns)
-- flatten' (Node l r) ns = flatten l ++ (flatten' r ns)
flatten' (Node l r) ns = flatten' l (flatten' r ns)

flat :: Tree -> [Int]
flat t = flatten' t []

-- source
data Expr = Val Int | Add Expr Expr

-- semantics
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

-- VM
type Stack = [Int]
type Code = [Op]
data Op = PUSH Int | ADD deriving Show

-- target
exec :: Code -> Stack -> Stack
exec []            s     = s
exec (PUSH n : cs) s     = exec cs (n : s)
exec (ADD : cs)    []    = error "to add stackshould contain 2+ elements"
exec (ADD : cs)    [_]   = error "to add stackshould contain 2+ elements"
exec (ADD : cs) (m:n:ss) = exec cs (n + m : ss)

compile :: Expr -> Code
compile (Val n)   = [PUSH n]
compile (Add x y) = compile x ++ compile y ++ [ADD]

e :: Expr
e = Add (Add(Val 2) (Val 3)) (Val 4)

r1 :: Int
r1 = eval e

code :: Code
code = compile e -- [PUSH 2,PUSH 3,ADD,PUSH 4,ADD]

r2 :: Stack
r2 = exec code []

proof1 :: Expr -> Bool
proof1 e = exec (compile e) [] == [eval e]

proof2 :: Expr -> Stack -> Bool
proof2 e s = exec (compile e) s == eval e : s