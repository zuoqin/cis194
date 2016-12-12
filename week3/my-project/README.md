# CIS 194 Week 2
## Algebraic data types

Often use this [pattern to convert IO -> not IO](http://stackoverflow.com/questions/1675366/a-haskell-function-of-type-io-string-string):
```haskell
readAndIndex fileName = do
    text <- readFile fileName
    return $ index text
```