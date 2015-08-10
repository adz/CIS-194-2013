fun1 :: [Integer] -> Integer
--fun1 [] = 1
--fun1 (x:xs)
--  | even x = (x - 2) * fun1 xs
--  | otherwise = fun1 xs
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
--fun2 1 = 0
--fun2 n | even n = n + fun2 (n ‘div‘ 2) -- always even
--       | otherwise = fun2 (3 * n + 1)  --
fun2 n = sumEvens (if odd n then 3 * n + 1 else n)
  where sumEvens = sum . takeWhile (> 0) . iterate (`div` 2)


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr appendedTree Leaf

appendedTree :: a -> Tree a -> Tree a
appendedTree newEl Leaf = Node 0 Leaf newEl Leaf
appendedTree newEl (Node h left oldEl right) = case ordering of
    LT -> buildTree (appendedTree newEl left) oldEl right
    _  -> buildTree left oldEl (appendedTree newEl right)
  where ordering = compare (treeHeight left) (treeHeight right)

buildTree l el r = Node h l el r
  where h = 1 + max (treeHeight l) (treeHeight r)

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node h _ _ _) = h
