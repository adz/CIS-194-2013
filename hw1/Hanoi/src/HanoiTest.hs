{-# OPTIONS_GHC -F -pgmF htfpp #-}
module HanoiTest where

import Hanoi
import Test.Framework

test_HanoiWithTwoDisks :: IO ()
test_HanoiWithTwoDisks = do
  assertEqual (hanoi 21 "a" "b" "c") [("a","c"), ("a","b"), ("c","b")]

