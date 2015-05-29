{-# OPTIONS_GHC -F -pgmF htfpp #-}
module ValidatingCreditCardNumbersTest where

import ValidatingCreditCardNumbers
import Test.Framework

test_ToDigits = do
  assertEqual ([1,0] :: [Integer]) (toDigits 10)
  assertEqual ([] :: [Integer]) (toDigits 0)

test_IsValid = do
  assertBool $ validate 4012888888881881
  assertBool $ not $ validate 4012888888881882
