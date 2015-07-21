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

