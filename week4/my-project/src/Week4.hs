-- CIS 194 Homework 4
module Week4 ( fun1,
               fun2,

               fun3,
               fun4,

               xor,
               map',
               sieve_of_Sundaram
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

--Exercise 3: More folds!
fold :: b -> (a -> b -> b) -> [a] -> b
fold z f []     = z
fold z f (x:xs) = f x (fold z f xs)

xor :: [Bool] -> Bool
xor []     = False
xor (x:xs) = case (fold 0 (\x s ->  if x == True then 1 + s else 0 + s) (x:xs)) `mod` 2 of
        1 -> True
        0 -> False


map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

--Exercise 4: Finding primes
sSundDelete :: Integer -> [Integer]            
sSundDelete n = [i+j+2*i*j|i<-[1..n], j<-[i..n]]

sieve_of_Sundaram :: Integer -> [Integer]
sieve_of_Sundaram n =
  let del = sSundDelete n in
     2:[2*x+1 | x <- [1..n], not (x `elem` del)]

     
