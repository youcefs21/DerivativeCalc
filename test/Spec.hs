
import Test.QuickCheck

import Derivative (diff)
import Expression


main :: IO ()
main = quickCheck evalDiffProp1


{-
  ======================================================================
  evaluates a mathematical expression at a given value `v` by 
  substituting the variable `X` with the value `v`
  ======================================================================
-}
eval :: (Floating a, Eq a) => MathExpr a -> a -> a
-- pattern matching unary operations
eval (Func1 Cos e) v       = cos (eval e v)
eval (Func1 Sin e) v       = sin (eval e v)
eval (Func1 Abs e) v       = abs (eval e v)
eval (Func1 (Power n) e) v = eval e v ^^ n
-- pattern matching binary operations
eval (Func2 Add e1 e2) v   = eval e1 v + eval e2 v
eval (Func2 Mult e1 e2) v  = eval e1 v * eval e2 v
-- pattern matching constants
eval (Coef c) v            = c
-- substituting X with v
eval X v                   = v


-- almost equal helper operator
infix 4 =~
(=~) :: (Floating a,Ord a) => a -> a -> Bool
x =~ y = abs (x - y) <= 1e-4


{-
  ======================================================================
  Diff Test Cases
  ======================================================================
-}

-- Test 1
evalDiffProp1 :: (Double,Double) -> Bool
evalDiffProp1 (x,y) = 2*x*(y*cos(y ^^ 2) - sin y) =~ eval (diff (Coef x * sin (X ^^ 2) + Coef 2*Coef x*cos X)) y
