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
    moveAllButOneFromAtoCUsingB = hanoiAllButOne pegA pegC pegB
    moveTopFromAToB = [(pegA, pegB)]
    moveAllButOneFromCtoBUsingA = hanoiAllButOne pegC pegB pegA
    hanoiAllButOne = hanoi (numberOfDisks - 1)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 numberOfDisks pegA pegB pegC pegD =
  moveTopHalfToC ++ moveBottomHalfToD ++
    moveTopFromAToB ++
      moveBottomHalfToB ++ moveTopHalfToB
  where
    moveTopHalfToC = hanoi4 topHalf pegA pegC pegB pegD
    moveBottomHalfToD = hanoi4 bottomHalf pegA pegD pegB pegC
    moveTopFromAToB = [(pegA, pegB)]
    moveBottomHalfToB = hanoi4 bottomHalf pegD pegB pegC pegA
    moveTopHalfToB = hanoi4 topHalf pegC pegB pegD pegA

    topHalf = quot allButOne 2
    bottomHalf = allButOne - topHalf
    allButOne = numberOfDisks - 1
