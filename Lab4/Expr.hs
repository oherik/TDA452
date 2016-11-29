

--TODO should we have cos and sin as expressions?
---- B ----
showExpr :: Expr -> String
showExpr (Num n) = showNum n
showExpr (Var v) = [v]    -- om Var sparas som en Char
showExpr (Cos e) = "cos " ++ showFact e
showExpr (Sin e) = "sin " ++ showFact e
showExpr (Add e1 e2) = showAdd e1 e2
showExpr (Mul e1 e2) = showMul e1 e2

showFact :: Expr -> String
showFact (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFact e = showExpr e

-- Display numbers without decimal points if they are integers
showNum :: Float -> String
showNum n | (n == fromIntegral (round n)) = show $ round n
          | otherwise = show n

showMul :: Expr -> Expr -> String
showMul (Num 0) _ = showFact (Num 0)
showMul _ (Num 0) = showFact (Num 0)
showMul (Num 1) e = showFact e
showMul e (Num 1) = showFact e
showMul (Var x) e = showFact e ++ "*" ++ showFact (Var x)
showMul e1 e2 = showFact e1 ++ "*" ++ showFact e2

showAdd :: Expr -> Expr -> String
showAdd (Num 0) e = showExpr e
showAdd e (Num 0) = showExpr e
showAdd e1 e2 = showExpr e1 ++ " + " ++ showExpr e2
