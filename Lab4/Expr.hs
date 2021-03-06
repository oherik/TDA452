module Expr where

import Parsing
import Data.Char
import Data.Maybe

---- A ----

data Expr = Num Double
            | Var Char
            | Opr Operators Expr Expr
            | Func Function Expr
            deriving(Eq)

data Operators = Mul | Add
          deriving(Eq)

data Function = Function { name :: String, function :: (Double -> Double)}

sin' = Function "sin" sin
cos' = Function "cos" cos

instance Eq Function where
   (Function name1 _) == (Function name2 _) = name1 == name2

instance Show Expr where
  show = showExpr

---- B ----
showExpr :: Expr -> String
showExpr e = showExpr' e
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
showArg e@(Num _) =  showExpr e
showArg e@(Var _) =  showExpr e
showArg e =  "(" ++ showExpr e ++ ")"

---- C ----
-- Given an expression and a value,
-- Calculates the value of the expression
eval :: Expr -> Double -> Double
eval (Num n) _ = n
eval (Var _) x = x
eval (Opr Add e1 e2) x = (eval e1 x) + (eval e2 x)
eval (Opr Mul e1 e2) x = (eval e1 x) * (eval e2 x)
eval (Func f e) x = evalFun f e x
  where
    evalFun :: Function -> Expr -> Double -> Double
    evalFun (Function _ f) e x = f $ eval e x

---- D ----
-- Parse a string, sorting out all spaces.
readExpr :: String -> Maybe Expr
readExpr s | rest == "" = Just e
           | otherwise = Nothing
  where
    noSpace = filter (/= ' ') s
    (e,rest) = fromJust $ parse expr noSpace

expr, expr', func, term, term', factor, factor', num, var :: Parser Expr
expr = expr' <|> term
expr'= do t <- term
          char '+'
          e <- expr
          return (Opr Add t e)

func = do f <- funP
          fac <- factor
          return (Func f fac)

term = term' <|> factor
term'=  do f <- factor
           char '*'
           t <- term
           return (Opr Mul f t)

factor = func <|> num <|> factor' <|> var
factor'= do char '('
            e <- expr
            char ')'
            return e

num = do a <- readsP ::  Parser Double
         return (Num (a))

var = do v <- charP
         return (Var v)

-- Parse if a character is a letter
charP :: Parser Char
charP = sat isLetter

-- Parse a predefined function
funP, sinP, cosP :: Parser Function
funP = sinP <|> cosP
sinP = do string "sin"
          return (Function "sin" sin)
cosP = do string "cos"
          return (Function "cos" cos)

-- Parse a word
string :: String -> Parser String
string [] = return ""
string (x:xs) = do  first <- char x
                    others <- string xs
                    return (first:others)

---- E ----
-- Moved to ExprQC.hs

---- F ----

-- Simplifies an expression (eg (Add (Num 3) (Num 4)) becomes
-- (Num 7))
simplify :: Expr -> Expr
simplify (Opr Mul e1 e2) = mul (simplify e1) (simplify e2)
simplify (Opr Add e1 e2) = add (simplify e1) (simplify e2)
simplify (Func f e) = fun f (simplify e)
simplify e = e

prop_simplify :: Expr -> Double -> Bool
prop_simplify e x = eval (simplify e) x == eval e x

-- Simplifies a function
fun :: Function -> Expr -> Expr
fun (Function _ f) (Num n) = Num (f n)
fun f e = Func f e

-- Simplifies a multiplication. Changes the order to make it more readable, eg
-- x*3 becomes 3*x
mul :: Expr -> Expr -> Expr
mul (Num 0) _   = Num 0
mul _ (Num 0)   = Num 0
mul (Num 1) e   = e
mul e (Num 1)   = e
mul (Num n1) (Num n2) = Num (n1*n2)
mul v@(Var _) e   = mul e v
mul f@(Func _ _) n@(Num _)  = mul n f
mul n@(Num n1) (Opr Mul e1 e2) = mul (mul n e1) e2
mul n@(Num n1) (Opr Add e1 e2) = add (mul n e1) (mul n e2)
mul e1 e2       = Opr Mul e1 e2

-- Simplifies an addition
add :: Expr -> Expr -> Expr
add (Num 0) e   = e
add e (Num 0)   = e
add (Num n1) (Num n2) = Num (n1+n2)
add (Opr Mul (Num n) e1) e2 | e1 == e2 = mul (Num (n+1)) e1
add e1 (Opr Add e2 e3) | e1 == e2 = add (mul (Num 2) e1) e3
                       | e1 == e3 = add e2 (mul (Num 2) e1)
add e1 e2       = Opr Add e1 e2

---- G ----
-- Differentiates an express with respect to x
differentiate :: Expr -> Expr
differentiate (Num n) = Num 0
differentiate (Var x) =  if x == 'x' then Num 1 else Num 0
differentiate (Opr Add e1 e2) = Opr Add (differentiate e1) (differentiate e2)
differentiate (Opr Mul e1 e2) = Opr Add (Opr Mul e1 (differentiate e2)) (Opr Mul (differentiate e1) e2)
differentiate (Func f e) = Opr Mul (differentiateFun f e) (differentiate e)
  where
    differentiateFun :: Function -> Expr -> Expr
    differentiateFun (Function "cos" _) e = Opr Mul (Num (-1)) (Func sin' e)
    differentiateFun (Function "sin" _) e = Func cos' e
