module Simplify where

import Expression


{-
  ======================================================================
  simplifies redundant parts of the equation 

  - equations multiplied by 0 become 0
  - multiplication by 1 removes the multiplication
  - addition by 0 removes the addition
  
  more simplifications will be added later
  ======================================================================
-}
simplify :: (Floating a, Eq a) => MathExpr a -> MathExpr a
-- simplify multiplication
  -- Example: (3 * x) * (x * 0) = 0
simplify (Func2 Mult ex1 ex2)
  | ex1 == Coef 0 = Coef 0
  | ex2 == Coef 0 = Coef 0
  | ex1 == Coef 1 = ex2S
  | ex2 == Coef 1 = ex1S
  | isOneZr = simplify $ ex1S * ex2S
  | isPowerEq ex1S ex2S  = addExpnt ex1S ex2S
  | otherwise = ex1S * ex2S
    where
      ex1S = simplify ex1
      ex2S = simplify ex2
      isOneZr = ex1S == 1 || ex1S == 0 || ex2S == 1 || ex2S == 0
      
      -- check that the two expressions are powers of the same expression
      isPowerEq :: (Floating a, Eq a) => MathExpr a -> MathExpr a -> Bool
      isPowerEq (Func1 (Power n) exp1) (Func1 (Power m) exp2) = exp1 == exp2
      isPowerEq ex1 (Func1 (Power m) exp2) = ex1 == exp2
      isPowerEq (Func1 (Power n) exp1) ex2 = exp1 == ex2
      isPowerEq ex1 ex2 = ex1 == ex2

      -- add the exponent of the two expressions assuming they are powers of the same expression
      addExpnt :: (Floating a, Eq a) => MathExpr a -> MathExpr a -> MathExpr a
      addExpnt (Func1 (Power n) exp1) (Func1 (Power m) exp2) = Func1 (Power (n + m)) exp1
      addExpnt (Func1 (Power n) exp1) ex2 = Func1 (Power (n + 1)) exp1
      addExpnt ex1 (Func1 (Power n) exp2) = Func1 (Power (n + 1)) exp2
      addExpnt ex1 ex2 = Func1 (Power 2) ex1

-- simplify addition
  -- Example: (x + 0) + (0 + x) = x + x
simplify (Func2 Add ex1 ex2)
  | ex1 == Coef 0 = simplify ex2
  | ex2 == Coef 0 = simplify ex1
  | isZr = simplify $ Func2 Add ex1S ex2S
  | otherwise = Func2 Add ex1S ex2S
    where
      ex1S = simplify ex1
      ex2S = simplify ex2
      isZr = ex1S == 0 || ex2S == 0

-- simplify exponentiation by 1 and 0
  -- Example: (x^2)^1 = x^2
simplify (Func1 (Power n) ex)
  | n == 0 = Coef 1
  | n == 1 = simplify ex
  | otherwise = Func1 (Power n) (simplify ex)

-- everything else is left as is
simplify ex = ex



