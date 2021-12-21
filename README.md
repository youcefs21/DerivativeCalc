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
    - `MathExpr` is an instance of the Num, Floating, and Fractional classes
- A function `diff` that can differentiate any expression that can be represented with `MathExpr`

