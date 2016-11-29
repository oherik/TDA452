
--TODO should we have cos and sin as expressions?
---- B ----
showExpr :: Expr -> String
showExpr (Num n) = showNum n
showExpr (Var v) = [v]    -- om Var sparas som en Char
showExpr (Cos e) = showCos $ simplified (Cos e)
showExpr (Sin e) = showSin $ simplified (Sin e)
showExpr (Add e1 e2) = showAdd $ simplified (Add e1 e2)
showExpr (Mul e1 e2) = showMul $ simplified (Mul e1 e2)

simplified :: Expr -> Expr
simplified (Mul e1 e2) = mul (simplified e1) (simplified e2)
simplified (Add e1 e2) = add (simplified e1) (simplified e2)
simplified (Sin e) = sin' (simplified e)
simplified (Cos e) = cos' (simplified e)
simplified e = e

showCos :: Expr -> String
showCos (Cos e)= "cos " ++ showArg (simplified e)
showCos e = showExpr e

showSin :: Expr -> String
showSin (Sin e)= "sin " ++ showArg (simplified e)
showSin e = showExpr e

showFact :: Expr -> String
showFact (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFact e = showExpr e

-- Display numbers without decimal points if they are integers
showNum :: Float -> String
showNum n | (n == fromIntegral (round n)) = show $ round n
          | otherwise = show n

showAdd :: Expr -> String
showAdd (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showAdd e = showExpr e

showMul :: Expr -> String
showMul (Mul e1 e2) = showFact e1 ++ "*" ++ showFact e2
showMul e = showExpr e

showArg :: Expr -> String
showArg (Num n) = showExpr (Num n)
showArg (Var x) = showExpr (Var x)
showArg e =  "(" ++ showExpr e ++ ")"

mul :: Expr -> Expr -> Expr
mul (Num 0) _ = (Num 0)
mul _ (Num 0) = (Num 0)
mul (Num 1) e = e
mul e (Num 1) = e
mul (Num n1) (Num n2) = (Num (n1*n2))
mul (Var x) e = (Mul e (Var x))
mul e1 e2 = (Mul e1 e2)

add :: Expr -> Expr -> Expr
add (Num 0) e = e
add e (Num 0) = e
add (Num n1) (Num n2) = (Num (n1+n2))
add e1 e2 = (Add e1 e2)

sin' :: Expr -> Expr
sin' (Num n) = Num(sin n)
sin' e = (Sin e)

cos' :: Expr -> Expr
cos' (Num n) = Num(cos n)
cos' e = (Cos e)
