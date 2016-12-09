-- Validating Credit Card Numbers1

module Hanoi
    ( hanoi
    ) where



type Peg = String
type Move = (Peg, Peg)

moveDisk :: Peg -> Peg -> Move
moveDisk a b = (a,b)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi height fromPole toPole withPole
  | height <= 0    = []
  | otherwise     = 
      let result1 = hanoi (height-1) fromPole withPole toPole
          result2 = hanoi (height-1) withPole toPole fromPole
      in result1 ++ [(fromPole, toPole)] ++ result2