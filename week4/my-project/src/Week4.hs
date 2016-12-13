-- CIS 194 Homework 3
module Week4 ( fun1,
               fun2,

               fun3,
               fun4
    ) where


--Exercise 1: Wholemeal programming
fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x - 2) . filter (even) 

--This seems straightforward enough, but it is not good Haskell style. The problem is that it is
--  doing too much at once; and
--  working at too low of a level.
fun3 :: [Integer] -> Integer
fun3 [] = 1
fun3 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs


fun2 :: Integer -> Integer
fun2 = sum . takeWhile (> 1) . iterate (`div` 2) . (\x ->  (x `mod` 2) *  (3 * x + 1) + (1 - (x `mod` 2)) * x)


fun4 :: Integer -> Integer
fun4 1 = 0
fun4 n
  | even n = n + fun4 (n `div` 2)
  | otherwise = fun4 (3 * n + 1)

