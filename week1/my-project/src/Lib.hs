module Lib
    ( someFunc,
   	  toDigits
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc111"

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

-- doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther = putStrLn "someFunc222"

-- toDigitsRev :: Integer -> [Integer]
-- toDigits = putStrLn "someFunc222"

-- sumDigits :: [Integer] -> Integer
-- sumDigits = putStrLn "someFunc222"