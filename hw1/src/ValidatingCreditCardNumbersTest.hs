{-# OPTIONS_GHC -F -pgmF htfpp #-}
module ValidatingCreditCardNumbersTest where

import ValidatingCreditCardNumbers
import Test.Framework

testToDigits :: IO ()
testToDigits = do
  assertEqual ([1,0] :: [Integer]) (toDigits 10)
  assertEqual ([] :: [Integer]) (toDigits 0)

testIsValid :: IO ()
testIsValid = do
  assertBool $ validate 4012888888881881
  assertBool $ not $ validate 4012888888881882
