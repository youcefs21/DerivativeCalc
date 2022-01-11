# Changelog for DerivativeCalc

## Initial implementation:

### Version 0.1.0
- Initial release
- Implemented `MathExpr`

### Version 0.1.1:
- Updated package.yaml with up to date project information

### Version 0.1.2:
- Fixed bad license formatting in package.yaml that caused project to not compile

### Version 0.2.0:
- Created a new module: `Derivative.hs`
    - Created a new function: `diff :: MathExpr -> MathExpr` which returns the derivative of a given expression
- Updated `Main.hs` to include the new module as an import
- Removed Signum from `MathExpr` because it doesn't really make any sense in the context of differentiation

### Version 0.2.1:
- Updated `README.md` to include the new features

### Version 0.2.2:
- Updated `ChangeLog.md` to include day count

### Version 0.3.0:
- Created a new module: `Pretty.hs`
    - Created a new function: `simplify :: MathExpr -> MathExpr` which does a very basic simplification of an expression
- Updated `Main.hs` to include the new module as an import
- Updated `Derivative.hs`'s `diff` function to use `simplify` before returning the derivative
- reformated version number from `x.y.z.w` to `x.y.z`

### Version 0.4.0:
- Created a new function: `prettyPrint :: MathExpr -> String` which converts an expression to a string with conventional notation
- Updated README.md to include the new features

### Version 0.4.1:
- renamed `Pretty.hs` to `Simplify.hs`
- `prettyPrint` has been removed, the implementation is now used to make `MathExpr` an instance of `Show` (in `Expression.hs`)

### Version 0.4.2:
- Added the following simplifications to `simplify`
    - `exp^n * exp^m = exp^(n+m)` (expressions with no exponentiation are assumed to have an exponent of 1)
    - `exp^0 = 1`
    - `exp^1 = exp`
- Updated `README.md` to list the simplifications that `simplify` performs
- Cleaned up `simplify`'s implementation

### Version 0.4.3:
- undid much of 0.4.1 because I realized that it's a bad idea to manually implement `Show` and `Read` for `MathExpr` instead of using the `deriving` keyword
- the function `prettyPrint` is now in `Expression.hs`

### Version 1.0.0:
- Created a new function `parseMathExpr` in `Expressions.hs` which parses a string into an expression
- Running `stack run` will now prompt for an expression to differentiate
- Updated `README.md` to include usage instructions and new features