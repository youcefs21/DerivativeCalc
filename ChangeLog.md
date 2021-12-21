# Changelog for DerivativeCalc

## Initial implementation

### Version 0.1.0
- Initial release
- Implemented `MathExpr`

### Version 0.1.0.1:
- Updated package.yaml with up to date project information
### Version 0.1.0.2:
- Fixed bad license formatting in package.yaml that caused project to not compile

### Version 0.2.0:
- Created a new module: `Derivative.hs`
    - Created a new function: `diff :: MathExpr -> MathExpr` which returns the derivative of a given expression
- Updated `Main.hs` to include the new module as an import
- Removed Signum from `MathExpr` because it doesn't really make any sense in the context of differentiation

### Version 0.2.0.1:
- Updated `README.md` to include the new features