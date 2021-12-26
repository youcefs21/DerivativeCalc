module Main where

import Expression
import Derivative
import Simplify

main :: IO ()
main = do
  putStrLn "Enter an expression:"
  input <- getLine
  let expr = parseMathExpr input
  putStrLn $ "Expression: " ++ prettyPrint (simplify expr)
  putStrLn $ "Derivative: " ++ prettyPrint (diff expr)
