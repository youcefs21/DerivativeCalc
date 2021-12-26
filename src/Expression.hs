module Expression where
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Text.Read (readMaybe)



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
  deriving (Show,Eq,Read)

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





{-
  ======================================================================
  pretty prints the equation
  ======================================================================
-}
prettyPrint :: (Show a) => MathExpr a -> String
prettyPrint X             = "X"
prettyPrint (Coef a)      = show a
prettyPrint (Func1 op ex) =
  case op of
    Cos     -> "cos(" ++ prettyPrint ex ++ ")"
    Sin     -> "sin(" ++ prettyPrint ex ++ ")"
    Abs     -> "abs(" ++ prettyPrint ex ++ ")"
    Power n -> "(" ++ prettyPrint ex ++ ")^" ++ show n
prettyPrint (Func2 op ex1 ex2) =
  case op of
    Add     -> "(" ++ prettyPrint ex1 ++ ") + (" ++ prettyPrint ex2 ++ ")"
    Mult    -> "(" ++ prettyPrint ex1 ++ ") * (" ++ prettyPrint ex2 ++ ")"


{-
  ======================================================================
  parses a string into a MathExpr
  ======================================================================
-}
parseMathExpr :: (Floating a, Eq a, Read a) => String -> MathExpr a
parseMathExpr s
  | "sin(" `isPrefixOf` s = parseBinOp (sin (parseMathExpr (inBracket (drop 4 s) 1)))  (outBracket (drop 4 s) 1)
  | "cos(" `isPrefixOf` s = parseBinOp (cos (parseMathExpr (inBracket (drop 4 s) 1)))  (outBracket (drop 4 s) 1)
  | "abs(" `isPrefixOf` s = parseBinOp (abs (parseMathExpr (inBracket (drop 4 s) 1)))  (outBracket (drop 4 s) 1)
  | "("    `isPrefixOf` s = parseBinOp (parseMathExpr (inBracket (drop 1 s) 1)) (outBracket (drop 1 s) 1)
  | "x"    `isPrefixOf` s = parseBinOp X (drop 1 s)
  | isJust (readMaybe s :: Maybe Double) =  Coef (read s)
  | otherwise = error "unrecognized expression"
  where
    inBracket :: String -> Int -> String
    inBracket (x:xs) n
      | x == ')' && n == 1 = ""
      | x == '(' = x:inBracket xs (n+1)
      | x == ')' = x:inBracket xs (n-1)
      | otherwise = x:inBracket xs n
    inBracket [] n = error "unbalanced brackets"

    outBracket :: String -> Int -> String
    outBracket (x:xs) n
      | x == ')' && n == 1 = xs
      | x == '(' = outBracket xs (n+1)
      | x == ')' = outBracket xs (n-1)
      | otherwise = outBracket xs n
    outBracket [] n = ""

    parseBinOp :: (Floating a, Eq a, Read a) => MathExpr a -> String -> MathExpr a
    parseBinOp ex s
      | "+" `isPrefixOf` s = ex + parseMathExpr (drop 1 s)
      | "-" `isPrefixOf` s = ex - parseMathExpr (drop 1 s)
      | "*" `isPrefixOf` s = ex * parseMathExpr (drop 1 s)
      | "/" `isPrefixOf` s = ex / parseMathExpr (drop 1 s)
      | "^" `isPrefixOf` s = Func1 (Power n) ex
      | otherwise = ex
        where
          n
            | isJust (readMaybe (tail s) :: Maybe Int) = read (tail s)
            | otherwise = error "non-integer power"