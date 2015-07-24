import Data.List

-- First go:
{-skips :: [a] -> [[a]]-}
{-skips xs = take (length xs) $ gen 0 xs-}

{-gen :: Integer -> [a] -> [[a]]-}
{-gen n xs = s n xs : gen (n + 1) xs-}

{-s :: Int -> [a] -> [a]-}
{-s n xs = case drop n xs of-}
           {-[] -> []-}
           {-(x:xs') -> x : s n xs'-}

-- Second go:
{-skips :: [a] -> [[a]]-}
{-skips xs = take (length xs) $ map (`s` xs) [0..]-}

{-s :: Int -> [a] -> [a]-}
{-s n xs = case drop n xs of-}
           {-[] -> []-}
           {-(x:xs') -> x : s n xs'-}



{-gen n xs = unfoldr (\(n, xs) -> Just ((s n, xs), (s (n+1) xs)))-}

-- 3
skips :: [a] -> [[a]]
skips xs = map (`s` xs) [0..length xs - 1]

s :: Int -> [a] -> [a]
s n xs = case drop n xs of
           [] -> []
           (x:xs') -> x : s n xs'


-- unused... so far
-- thinking to get list of indexes, then map over that...
indexes :: Int -> Int -> Int -> [Int]
indexes cur inc tot
  | cur + inc < tot = cur + inc : indexes (cur + inc + 1) inc tot
  | otherwise = []




-- Ex2 Local Maxima
localMaxima :: [Integer] -> [Integer]
localMaxima l = concat $ zipWith3 m l (drop 1 l) (drop 2 l)

-- is b a local maxima?
m :: Integer -> Integer -> Integer -> [Integer]
m a b c = [b | b > a && b > c]



-- Ex3 Histogram
{-histogram :: [Integer] -> String-}
-- so far... this one converts to counts
histogram :: [Integer] -> String
histogram xs = intercalate "\n" $ graph ++ [key]
  where graph = map (`mapLine` counts) (maxDown counts)
        key = "==========\n0123456789"
        counts = map (`count` xs) [0..9]

maxDown :: [Int] -> [Int]
maxDown counts' = reverse [1 .. maximum counts']

mapLine :: Int -> [Int] -> String
mapLine line = map (toStar line)

toStar :: Int -> Int -> Char
toStar line count' = if count' >= line then '*' else ' '

count :: Integer -> [Integer] -> Int
count n = length . filter (n==)

