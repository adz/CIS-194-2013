{-# OPTIONS_GHC -F -pgmF htfpp #-}
module HanoiTest where

import Hanoi
import Test.Framework

test_HanoiWithTwoDisks :: IO ()
test_HanoiWithTwoDisks = do
  assertEqual expectedMoves (hanoi 2 "a" "b" "c") 
  where expectedMoves = [("a","c"), ("a","b"), ("c","b")]

test_HanoiWithThreeDisks :: IO ()
test_HanoiWithThreeDisks = do
  assertEqual expectedMoves (hanoi 3 "a" "b" "c")
  where expectedMoves = [("a", "b"), ("a", "c"), ("b", "c"), ("a", "b"), ("c", "a"), ("c", "b"), ("a", "b")]

test_MovesFor15Disks :: IO ()
test_MovesFor15Disks = do
  assertEqual 32767 movesFor15Disks
  where movesFor15Disks = length $ hanoi 15 "a" "b" "c"
