# Changelog for DerivativeCalc

## Initial implementation

## Day 1:

### Version 0.1.0
- Initial release
- Implemented `MathExpr`

## Day 2:

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

## Day 3:
I had a 24-hour exam, so no progress was made.


## Day 4:

### Version 0.2.2:

- Updated `ChangeLog.md` to include day count

### Version 0.3.0:
- Created a new module: `Pretty.hs`
    - Created a new function: `simplify :: MathExpr -> MathExpr` which does a very basic simplification of an expression
- Updated `Main.hs` to include the new module as an import
- Updated `Derivative.hs`'s `diff` function to use `simplify` before returning the derivative
- reformated version number from `x.y.z.w` to `x.y.z`

## Day 5:

### Version 0.4.0:
- Created a new function: `prettyPrint :: MathExpr -> String` which converts an expression to a string with conventional notation
- Updated README.md to include the new features

## Day 6:
### Version 0.4.1:
- renamed `Pretty.hs` to `Simplify.hs`
- `prettyPrint` has been removed, the implementation is now used to make `MathExpr` an instance of `Show` (in `Expression.hs`)
