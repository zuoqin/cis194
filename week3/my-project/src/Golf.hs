-- CIS 194 Homework 3
module Golf ( skips,
              localMaxima,
              histogram,

              count,
              maxoccur
    ) where

import Data.List

--Exercise 1 Hopscotch
myskip :: ([a],Int) -> [a]
myskip (lst,num)
  | num == 0 = lst
  | otherwise = map fst (filter (\(_, ind) -> (ind >= (num-1) && (ind+1) `mod` num == (0 :: Int)) )   (zip lst [0..]))

copylst :: [a] -> [([a],Int)]
copylst lst = map myfun (zip lst [1..])
  where myfun (x,y) = (lst,y)

skips :: [a] -> [[a]]

skips (h:xs) = map myskip (copylst (h:xs))
skips _ = []
 

--Exercise 2 Local maxima
localMaxima :: [Integer] -> [Integer]

localMaxima (h:xs) = map fst (filter (\(itm, ind) -> ( itm > ( (h:xs) !! (ind + 1) )) && (itm > ( (h:xs) !! (ind -1))))   (zip (take ((length xs) - 1)xs) [1..]))
localMaxima _ = []

--Exercise 3 Histogram

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

maxoccur :: [Integer] -> [Int]
maxoccur lst = map counting [0..9]
  where counting x = count x lst

buildstr :: Int -> [Int] -> [Char]
buildstr x lst =
  map tostr [0..9]
    where tostr y = if (lst !! y) >= x
                    then '*'
                    else ' '

buildup :: [Integer] -> [[Char]]
buildup lst = map strnum  [(- (maximum (maxoccur lst)))..(-1)]
  where strnum x = buildstr (- x) (maxoccur lst)

  
histogram :: [Integer] -> String
histogram lst = intercalate "\n" ((buildup lst) ++ ["=========", "0123456789"])
