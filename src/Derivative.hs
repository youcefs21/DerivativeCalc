module Derivative where

import Expression
import Simplify

{-
  ======================================================================
  symbolically computes the derivative of a mathematical expression, then
  basic simplification is applied to the result.
  ======================================================================
-}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X             = Coef 1
diff (Coef a)      = Coef 0
diff (Func1 op ex) = 
  case op of
    Cos     -> simplify $ negate (Func1 Sin ex) * diff ex
    Sin     -> simplify $ cos ex * diff ex
    Abs     -> simplify $ ex * recip (Func1 Abs ex) * diff ex
    Power n -> simplify $ Coef (fromIntegral n) * Func1 (Power (n-1)) ex * diff ex
diff (Func2 op ex1 ex2) = 
  case op of
    Add     -> simplify $ diff ex1 + diff ex2
    Mult    -> simplify $ diff ex1 * ex2 + ex1 * diff ex2