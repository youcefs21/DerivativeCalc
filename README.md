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
    - `MathExpr` is a custom instance of the `Num`, `Floating`, `Fractional`, `Show` classes
- A function `diff` that can differentiate any expression that can be represented with `MathExpr`
- A function `simplify` that can perform basic simplifications on expressions to reduce redundancy:
    - `exp * 0 = 0` and `0 * exp = 0`
    - `exp * 1 = exp` and `1 * exp = exp`
    - `exp + 0 = exp` and `0 + exp = exp`
    - `exp^n * exp^m = exp^(n+m)` (expressions with no exponentiation are assumed to have an exponent of 1)
    - `exp^0 = 1`
    - `exp^1 = exp`