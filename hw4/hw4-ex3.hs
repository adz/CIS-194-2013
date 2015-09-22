-- Ex3: Folds
--
-- True if odd number of True values. Must use a fold.
xor :: [Bool] -> Bool
xor = foldr f False
  where f True acc = not acc
        f False acc = acc

-- Implement map as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- Implement foldl using foldr
{-
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f base xs = foldr ..

-- in such a way that myFoldl behaves identically to the standard
-- foldl function.
-- Hint: Study how the application of foldr and foldl work out:
--
foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn
-}
