-- Ex4: Finding Primes
-- Given an integer n, generate all odd primes up to 2n + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) (sieved n)

sieved :: Integer -> [Integer]
sieved n = [ n' | n' <- [1..n]
                , i <- [1..]
                , j <- [1..]
                , i + j + 2 * i * j <= n'
                , 1 <= i
                , i <= j]
