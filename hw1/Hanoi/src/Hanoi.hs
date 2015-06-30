module Hanoi where

-- ===================================
-- Ex. 1
-- ===================================
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi numberOfDisks pegA pegB pegC =
  moveAllButOneFromAtoCUsingB ++ moveTopFromAToB ++ moveAllButOneFromCtoBUsingA
  where
    moveAllButOneFromAtoCUsingB = hanoi (numberOfDisks - 1) pegA pegC pegB
    moveTopFromAToB = [(pegA, pegB)]
    moveAllButOneFromCtoBUsingA = hanoi (numberOfDisks - 1) pegC pegB pegA

hanoi4 :: Integer -> Peg -> Peg -> Peg -> t -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 numberOfDisks pegA pegB pegC pegD =
  moveAllButOneFromAtoCUsingB ++ moveTopFromAToB ++ moveAllButOneFromCtoBUsingA
  where
    moveAllButOneFromAtoCUsingB = hanoi (numberOfDisks - 1) pegA pegC pegB
    moveTopFromAToB = [(pegA, pegB)]
    moveAllButOneFromCtoBUsingA = hanoi (numberOfDisks - 1) pegC pegB pegA
