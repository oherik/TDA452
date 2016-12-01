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

--TODO should we have cos and sin as expressions?
---- B ----
--TODO test av fancy syntax
showExpr :: Expr -> String
showExpr e = showSimplified $ simplified e
    where
      showSimplified :: Expr -> String
      showSimplified (Num n) = showNum n
      showSimplified (Var v) = [v]
      showSimplified (Func f e) = showFun f e
      showSimplified (Opr op e1 e2) = showOpr op e1 e2

-- Simplifies an expression (eg (Add (Num 3) (Num 4)) becomes
-- (Num 7))
simplified :: Expr -> Expr
simplified (Opr Mul e1 e2) = mul (simplified e1) (simplified e2)
simplified (Opr Add e1 e2) = add (simplified e1) (simplified e2)
simplified (Func Sin e) = sin' (simplified e)
simplified (Func Cos e) = cos' (simplified e)
simplified e = e

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

---- C ----
eval :: Expr -> Double -> Double
--
eval (Num n) _ = n
eval (Var a) x = x
eval (Opr Add e1 e2) x = (eval e1 x) + (eval e2 x)
eval (Opr Mul e1 e2) x = (eval e1 x) * (eval e2 x)
eval (Func Sin e) x = sin (eval e x)
eval (Func Cos e) x = cos (eval e x)

---- D ----
-- readExpr :: String -> Maybe Expr
--

---- E ----
-- prop_ShowReadExpr :: Expr -> Bool
-- arbExpr :: Int -> Gen Expr
--

---- F ----
-- simplify :: Expr -> Expr
--

---- G ----
-- differentiate :: Expr -> Expr
--
