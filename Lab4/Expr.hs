{-# LANGUAGE LambdaCase #-}

--TODO should we have cos and sin as expressions?
---- B ----
--TODO test av fancy syntax
showExpr :: Expr -> String
showExpr e = showSimplified $ simplified e
    where
      showSimplified :: Expr -> String
      showSimplified = \case
        Num n     -> showNum n
        Var v     -> [v]    -- om Var sparas som en Char
        Cos e     -> "cos " ++ showArg e
        Sin e     -> "sin " ++ showArg e
        Add e1 e2 -> showExpr e1 ++ " + " ++ showExpr e2
        Mul e1 e2 -> showFact e1 ++ "*" ++ showFact e2

-- Simplifies an expression (eg (Add (Num 3) (Num 4)) becomes
-- (Num 7))
simplified :: Expr -> Expr
simplified (Mul e1 e2) = mul (simplified e1) (simplified e2)
simplified (Add e1 e2) = add (simplified e1) (simplified e2)
simplified (Sin e) = sin' (simplified e)
simplified (Cos e) = cos' (simplified e)
simplified e = e

-- Convert a numerical expression to a string
-- Display numbers without decimal points if they are integers
showNum :: Float -> String
showNum n | (n == fromIntegral (round n)) = show $ round n
          | otherwise = show n

-- A factor that's an addition should have parentheses around it
showFact :: Expr -> String
showFact (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFact e = showExpr e

-- Arguments consisting of other functions or additions should have
-- parantheses around them
showArg :: Expr -> String
showArg (Num n) = showExpr (Num n)
showArg (Var v) = showExpr (Var v)
showArg e =  "(" ++ showExpr e ++ ")"

-- Simplifies a multiplication
mul :: Expr -> Expr -> Expr
mul (Num 0) _   = Num 0
mul _ (Num 0)   = Num 0
mul (Num 1) e   = e
mul e (Num 1)   = e
mul (Num n1) (Num n2) = Num (n1*n2)
mul (Var x) e   = Mul e (Var x)
mul e1 e2       = Mul e1 e2

-- Simplifies an addition
add :: Expr -> Expr -> Expr
add (Num 0) e   = e
add e (Num 0)   = e
add (Num n1) (Num n2) = Num (n1+n2)
add e1 e2       = Add e1 e2

-- Simplifies a sin expression
sin' :: Expr -> Expr
sin' (Num n)    = Num(sin n)
sin' e          = Sin e

-- Simplifies a cos expression
cos' :: Expr -> Expr
cos' (Num n)    = Num(cos n)
cos' e          = Cos e
