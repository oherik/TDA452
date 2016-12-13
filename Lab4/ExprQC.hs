import Expr
import Test.QuickCheck
import Data.Maybe

-- Compares the strings produced by both showing the expression and by reading
-- and again showing the same expression
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr ex = showExpr ex ==
  (showExpr $ fromJust $ readExpr $ showExpr ex)

-- Create an arbitrary expression.
-- Only Num and Var count towards the length (excluding the remainder)
arbExpr :: Int -> Gen Expr
arbExpr 1     = frequency [(4,rNum), (2,return (Var 'x')), (1,rFunc)]
  where
      rNum = elements [Num n | n <- [0..10] :: [Double]] -- TODO float values?
      rFunc = do fun <- elements [sin', cos']
                 arg <- arbExpr 1
                 return (Func fun arg)
arbExpr size  = rOpr
  where
    rOpr = do op <- elements [Add,Mul]
              left <- arbExpr s1
              right <- arbExpr s2
              return (Opr op left right)
    s1 = size `div` 2
    s2 = size - s1

instance Arbitrary Expr where
  arbitrary = sized arbExpr
