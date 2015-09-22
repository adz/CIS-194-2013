-- Ex2 Folding with trees
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

