module ExprQC where

import Expr
import Test.QuickCheck
import Data.Maybe

-- TODO do like this?
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr ex = showExpr ex ==
  (showExpr (fromJust ((readExpr (showExpr ex)))))

-- TODO: or like this?
-- prop_ShowReadExpr :: Expr -> Property
 --prop_ShowReadExpr ex = (fromJust $ readExpr (showExpr ex)) == ex		 +prop_ShowReadExpr ex = forAll rNum (\ x -> eval (fromJust (readExpr (showExpr ex))) x `almostEqual` eval ex x)
 --		  where
 --   almostEqual :: Double -> Double -> Bool
 --   almostEqual x y = abs (x - y) < 0.001
 --
 --rNum :: Gen Double
 --rNum = do n <- arbitrary
 --          return n

-- Only Num and Var count towards the length (excluding the remainder)
arbExpr :: Int -> Gen Expr
arbExpr size = frequency [(4,rNum),
                (2,return (Var 'x')),(2*size,rOpr),(size,rFunc)]
  where
    rNum = elements [Num n | n <- [0..10] :: [Double]] -- TODO float values?
    rOpr = do op <- elements [Add,Mul]
              let size' = size `div` 2
              left <- arbExpr size'
              right <- arbExpr size'
              return (Opr op left right)
    rFunc = do fun <- elements [sin', cos']
               arg <- arbExpr size
               return (Func fun arg)

instance Arbitrary Expr where
  arbitrary = sized arbExpr
