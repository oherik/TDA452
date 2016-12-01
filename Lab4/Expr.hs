---- A ----

data Expr = Num Double
            | Var Char
            | Opr Operators Expr Expr
            | Func Functions Expr
            deriving (Eq)

data Operators = Mul | Add
                deriving (Eq)

data Functions = Sin | Cos
                deriving (Eq)


---- B ----

showExpr :: Expr -> String
showExpr e = showExpr' $ simplify e
    where
      showExpr' :: Expr -> String
      showExpr' (Num n) = showNum n
      showExpr' (Var v) = [v]
      showExpr' (Func f e) = showFun f e
      showExpr' (Opr op e1 e2) = showOpr op e1 e2


-- Convert a numerical expression to a string
-- Display numbers without decimal points if they are integers
showNum :: Double -> String
showNum n | (n == fromIntegral (round n)) = show $ round n
          | otherwise = show n

-- Convert a function expression to a string
showFun :: Functions -> Expr -> String
showFun Cos e = "cos " ++ showArg e
showFun Sin e = "sin " ++ showArg e

-- Convert an operation expression to a string
showOpr :: Operators -> Expr -> Expr -> String
showOpr Mul e1 e2 = showFact e1 ++ "*" ++ showFact e2
showOpr Add e1 e2 = showExpr e1 ++ " + " ++ showExpr e2

-- A factor that's an addition should have parentheses around it
showFact :: Expr -> String
showFact (Opr Add e1 e2) = "(" ++ showExpr (Opr Add e1 e2) ++ ")"
showFact e = showExpr e

-- Arguments consisting of other functions or additions should have
-- parantheses around them
showArg :: Expr -> String
showArg (Num n) = showExpr (Num n)
showArg (Var v) = showExpr (Var v)
showArg e =  "(" ++ showExpr e ++ ")"


---- C ----

eval :: Expr -> Double -> Double
eval (Num n) _        = n
eval (Var _) x        = x
eval (Opr o e1 e2) x  = evalOpr o e1 e2 x
eval (Func f e) x     = evalFunc f e x

evalOpr :: Operators -> Expr -> Expr -> Double -> Double
evalOpr Add e1 e2 x = (eval e1 x) + (eval e2 x)
evalOpr Mul e1 e2 x = (eval e1 x) * (eval e2 x)

evalFunc :: Functions -> Expr -> Double -> Double
evalFunc Sin e x = sin (eval e x)
evalFunc Cos e x = cos (eval e x)


---- F ----

-- Simplifies an expression (eg (Add (Num 3) (Num 4)) becomes
-- (Num 7))
simplify :: Expr -> Expr
simplify (Opr Mul e1 e2) = mul (simplify e1) (simplify e2)
simplify (Opr Add e1 e2) = add (simplify e1) (simplify e2)
simplify (Func Sin e) = sin' (simplify e)
simplify (Func Cos e) = cos' (simplify e)
simplify e = e

-- Simplifies a multiplication
mul :: Expr -> Expr -> Expr
mul (Num 0) _   = Num 0
mul _ (Num 0)   = Num 0
mul (Num 1) e   = e
mul e (Num 1)   = e
mul (Num n1) (Num n2) = Num (n1*n2)
mul (Var x) e   = Opr Mul e (Var x)
mul e1 e2       = Opr Mul e1 e2

-- Simplifies an addition
add :: Expr -> Expr -> Expr
add (Num 0) e   = e
add e (Num 0)   = e
add (Num n1) (Num n2) = Num (n1+n2)
add e1 e2       = Opr Add e1 e2

-- Simplifies a sin expression
sin' :: Expr -> Expr
sin' (Num n)    = Num(sin n)
sin' e          = Func Sin e

-- Simplifies a cos expression
cos' :: Expr -> Expr
cos' (Num n)    = Num(cos n)
cos' e          = Func Cos e
