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

test_Hanoi4WithTwoDisks :: IO ()
test_Hanoi4WithTwoDisks = do
  assertEqual 3 movesFor3Disks
  where movesFor3Disks = length $ hanoi4 2 "a" "b" "c" "d"

test_Hanoi4WithThreeDisks :: IO ()
test_Hanoi4WithThreeDisks = do
  assertEqual 5 movesFor3Disks
  where movesFor3Disks = length $ hanoi4 3 "a" "b" "c" "d"

test_OutputOfHanoi4:: IO ()
test_OutputOfHanoi4 = do
  assertEqual moves (hanoi4 4 "a" "b" "c" "d")
  where moves = []

test_Hanoi4MovesFor15Disks :: IO ()
test_Hanoi4MovesFor15Disks = do
  assertEqual 129 movesFor15Disks
  where movesFor15Disks = length $ hanoi4 15 "a" "b" "c" "d"


