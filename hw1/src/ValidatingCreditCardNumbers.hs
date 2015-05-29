module ValidatingCreditCardNumbers where

-- ===================================
-- Ex. 1
-- ===================================

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x
  | x >= 0 = toDigits (x `div` 10) ++ [x `mod` 10]
  | x < 0 = error "No negatives"

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- ===================================
-- Ex. 2
-- ===================================

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:x':xs) = [x, x' + x'] ++ doubleEveryOther xs


-- ===================================
-- Ex. 3
-- ===================================

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ concatMap toDigits xs


-- ===================================
-- Ex. 4
-- ===================================

validate :: Integer -> Bool
validate x = calculation `mod` 10 == 0
  where calculation = (sumDigits . doubleEveryOther . toDigitsRev) x

