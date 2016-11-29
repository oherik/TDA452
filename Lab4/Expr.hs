
---- B ----
showExpr :: Expr -> String
showExpr (Num n) = showNum n
showExpr (Var v) = [v]    -- om Var sparas som en Char
showExpr (Fun f e) = f ++ " " ++ showArg e  -- om Fun sparas som en String
showExpr (Add e1 e2) = showAdd e1 e2
showExpr (Mul e1 e2) = showMul e1 e2

showFact :: Expr -> String
showFact (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFact e = showExpr e

showArg :: Expr -> String
showArg (Num n) = showExpr (Num n) ++ " "
showArg (Var v) = showExpr (Var v) ++ " "
showArg e = "(" ++ showExpr e ++ ")"

showNum :: Float -> String
showNum n | (n == fromIntegral (round n)) = show $ round n
          | otherwise = show n

showMul :: Expr -> Expr -> String
showMul (Num 0) _ = "0"
showMul _ (Num 0) = "0"
showMul (Num 1) e = showExpr e
showMul e (Num 1) = showExpr e
showMul e1 e2 = showFact e1 ++ "*" ++ showFact e2

showAdd :: Expr -> Expr -> String
showAdd (Num 0) e = showExpr e
showAdd e (Num 0) = showExpr e
showAdd e1 e2 = showExpr e1 ++ "+" ++ showExpr e2
