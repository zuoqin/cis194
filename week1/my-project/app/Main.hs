module Main where

import Creditcard
import Hanoi

--x = validate( sumDigits( reverse( doubleEveryOther( toDigitsRev (4012888888881881) ) ) ) )
x = hanoi 2 "a" "b" "c"
main :: IO ()


main = print x
