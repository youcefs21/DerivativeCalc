module Expression where



{-
  ======================================================================
  An Abstract Syntax Tree for representing mathematical expressions.

  A mathematical expression is either
  - a variable, 
  - a constant, 
  - a unary operator applied to another mathematical expression, or 
  - a binary operator applied to two other mathematical expressions.
  ======================================================================
-}
data MathExpr a =
  X
  | Coef a
  | Func1 UnaryOp (MathExpr a)
  | Func2 BinOp (MathExpr a) (MathExpr a)
  deriving (Eq,Show,Read)

data BinOp = Add | Mult 
  deriving (Show,Eq,Read)

data UnaryOp = Cos | Sin | Abs | Power Int 
  deriving (Show,Eq,Read)


{-
  ======================================================================
  implements the Num type class for MathExpr
  ======================================================================
-}
instance Num a => Num (MathExpr a) where
  x + y         = Func2 Add x y
  x * y         = Func2 Mult x y
  negate x      = Func2 Mult (Coef (-1)) x
  abs x         = Func1 Abs x
  fromInteger i = Coef (fromInteger i)
  signum x      = error "will not be implemented"

{-
  ======================================================================
  implements the Fractional type class for MathExpr
  ======================================================================
-}
instance Fractional a => Fractional (MathExpr a) where
  recip e        = Func1 (Power (-1)) e
  fromRational e = Coef (fromRational e)


{-
  ======================================================================
  implements the Floating type class for MathExpr
  ======================================================================
-}
instance Floating a => Floating (MathExpr a) where
  pi      = Coef pi
  sin     = Func1 Sin
  cos     = Func1 Cos
  log     = error "log is left un-implemented for now"
  asin _  = error "asin is left un-implemented for now"
  acos _  = error "acos is left un-implemented for now"
  atan _  = error "atan is left un-implemented for now"
  sinh _  = error "sinh is left un-implemented for now"
  cosh _  = error "cosh is left un-implemented for now"
  tanh _  = error "tanh is left un-implemented for now"
  asinh _ = error "asinh is left un-implemented for now"
  acosh _ = error "acosh is left un-implemented for now"
  atanh _ = error "atanh is left un-implemented for now"
  exp _   = error "exp is left un-implemented for now"
  sqrt _  = error "sqrt is left un-implemented for now"