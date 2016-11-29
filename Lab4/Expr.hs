
---- B ----
showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (Var v) = [v]    -- om Var sparas som en Char
showExpr (Fun f e) = f ++ " " ++ showArg e -- om Fun sparas som en String
showExpr (Add e1 e2) = add e1 e2
showExpr (Mul e1 e2) = mul e1 e2

showFact :: Expr -> String
showFact (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFact e = showExpr e

showArg :: Expr -> String
showArg (Num n) = showExpr (Num n)
showArg (Var v) = showExpr (Var v)
showArg e = "(" ++ showExpr e ++ ")"

mul :: Expr -> Expr -> String
mul (Num 0) _ = "0"
mul _ (Num 0) = "0"
mul (Num 1) e = showExpr e
mul e (Num 1) = showExpr e
mul e1 e2 = showFact e1 ++ "*" ++ showFact e2

add :: Expr -> Expr -> String
add (Num 0) e = showExpr e
add e (Num 0) = showExpr e
add e1 e2 = showExpr e1 ++ "+" ++ showExpr e2
