module Pretty where

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
-- simplify multiplication by 0 and 1
  -- Example: (3 * x) * (x * 0) = 0
simplify (Func2 Mult ex1 ex2)
  | ex1 == Coef 0 = Coef 0
  | ex2 == Coef 0 = Coef 0
  | ex1 == Coef 1 = simplify ex2
  | ex2 == Coef 1 = simplify ex1
  | isOneZr = simplify $ Func2 Mult ex1S ex2S
  | otherwise = Func2 Mult ex1S ex2S
    where 
      ex1S = simplify ex1
      ex2S = simplify ex2
      isOneZr = ex1S == 1 || ex1S == 0 || ex2S == 1 || ex2S == 0
-- simplify addition by 0
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
-- everything else is left as is
simplify ex = ex

