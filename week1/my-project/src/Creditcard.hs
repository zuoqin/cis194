-- Validating Credit Card Numbers1

module Creditcard
    ( toDigits,
   	  toDigitsRev,
   	  doubleEveryOther,
   	  sumDigits,
   	  validate
    ) where

double_function :: (Integer,Int) -> Integer
double_function (x,y) = if y `mod` 2 == 1
					then (2 * x )
					else x


sumdigits_function :: Integer -> Integer
sumdigits_function x = if x > 10
					then x `mod` 10 + sumdigits_function (quot x 10)
					else x

array_to_index_tuple :: [Integer] -> [Integer]
array_to_index_tuple a = map double_function (zip a [0..])

toDigits :: Integer -> [Integer]
toDigits x
	| x <= 0 		= []
	| otherwise     = toDigits (x `div` 10) ++ [x `mod` 10]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther a = array_to_index_tuple a

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse( toDigits x )

sumDigits :: [Integer] -> Integer
sumDigits a = sum(map sumdigits_function a)

validate :: Integer -> Bool
validate x = if x `mod` 10 == 0
		then True
		else False