# Weekly Project #1: Derivative Calculator

A Haskell project that symbolically differentiates a given equation.

## Current Features:
- An algebraic data type `MathExpr`:
    - it can represent any single variable mathematical expression, as long as it only consists of the following operators:
        - addition
        - multiplication
        - exponentiation
        - sin
        - cos
        - absolute value
    - `MathExpr` is a custom instance of the Num, Floating, Fractional, show classes
- A function `diff` that can differentiate any expression that can be represented with `MathExpr`
- A function `simplify` that can perform basic simplifications on expressions with redundant multiplication by 1 or 0, and redundant addition of 0