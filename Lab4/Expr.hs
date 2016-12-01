---- A ----

data Expr = Num Double
            | Var Char
            | Opr Operators Expr Expr
            | Func Function Expr

data Operators = Mul | Add

data Function = Function { name :: String, function :: (Double -> Double)}

sin' = Function "sin" sin
cos' = Function "cos" cos

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
showFun :: Function -> Expr -> String
showFun (Function name _) e = name ++ " " ++ showArg e

-- Convert an operation expression to a string
showOpr :: Operators -> Expr -> Expr -> String
showOpr Mul e1 e2 = showFact e1 ++ "*" ++ showFact e2
showOpr Add e1 e2 = showExpr e1 ++ " + " ++ showExpr e2

-- A factor that's an addition should have parentheses around it
showFact :: Expr -> String
showFact e@(Opr Add e1 e2) = "(" ++ showExpr e ++ ")"
showFact e = showExpr e

-- Arguments consisting of other functions or additions should have
-- parantheses around them
showArg :: Expr -> String
showArg e@(Num _) = showExpr e
showArg e@(Var _) = showExpr e
showArg e =  "(" ++ showExpr e ++ ")"

---- C ----
-- Given an expression and a value,
-- Calculates the value of the expression
eval :: Expr -> Double -> Double
eval (Num n) _ = n
eval (Var a) x = x
eval (Opr Add e1 e2) x = (eval e1 x) + (eval e2 x)
eval (Opr Mul e1 e2) x = (eval e1 x) * (eval e2 x)
eval (Func f e) x = evalFun f e x

evalFun :: Function -> Expr -> Double -> Double
evalFun (Function name f) e x = f $ eval e x

---- D ----
-- readExpr :: String -> Maybe Expr
--

---- E ----
-- prop_ShowReadExpr :: Expr -> Bool
-- arbExpr :: Int -> Gen Expr
--

---- F ----

-- Simplifies an expression (eg (Add (Num 3) (Num 4)) becomes
-- (Num 7))
simplify :: Expr -> Expr
simplify (Opr Mul e1 e2) = mul (simplify e1) (simplify e2)
simplify (Opr Add e1 e2) = add (simplify e1) (simplify e2)
simplify (Func f e) = fun f (simplify e)
simplify e = e

-- Simplifies a function
fun :: Function -> Expr -> Expr
fun (Function _ f) (Num n) = Num (f n)
fun f e = Func f e

-- Simplifies a multiplication
mul :: Expr -> Expr -> Expr
mul (Num 0) _   = Num 0
mul _ (Num 0)   = Num 0
mul (Num 1) e   = e
mul e (Num 1)   = e
mul (Num n1) (Num n2) = Num (n1*n2)
mul v@(Var x) e   = Opr Mul e v
mul e1 e2       = Opr Mul e1 e2

-- Simplifies an addition
add :: Expr -> Expr -> Expr
add (Num 0) e   = e
add e (Num 0)   = e
add (Num n1) (Num n2) = Num (n1+n2)
add e1 e2       = Opr Add e1 e2

---- G ----
-- differentiate :: Expr -> Expr
--
