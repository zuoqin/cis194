module Main where

import LogAnalysis

x = validate( sumDigits( reverse( doubleEveryOther( toDigitsRev (4012888888881881) ) ) ) )

main :: IO ()


main = print x
