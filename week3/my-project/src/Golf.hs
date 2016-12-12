-- CIS 194 Homework 3

module Golf ( skips,
              localMaxima,
              histogram,

              count
    ) where

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

maxoccur :: [Integer] -> Integer
maxoccur lst = map counting [0..9]
  where counting x = count x lst

histogram :: [Integer] -> [Int]
histogram lst = map strnum  [(- (maximum (maxoccur lst)))..0]
  where strnum x = map maxoccur lst
