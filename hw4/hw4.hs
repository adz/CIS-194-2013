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
