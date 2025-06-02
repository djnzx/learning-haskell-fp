module Y15 where

import Y14PlusL2 (Expr (Add, Val), Stack, Code, Op (PUSH, ADD), eval, exec, compile)

compile' :: Expr -> Code -> Code
compile' (Val n) cs = PUSH n : cs
compile' (Add x y) cs = compile' x (compile' y (ADD : cs))

comp' :: Expr -> Code
comp' ex = compile' ex []

proof3 :: Expr -> Code -> Stack -> Bool
proof3 e c s = exec (compile' e c) s == exec c (eval e : s)
