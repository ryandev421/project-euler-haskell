module Problem_5
    ( solve
    ) where

primes = [2, 3, 5, 7, 11, 13, 17, 19]
productOfPrimes = product primes

isDivisible  n = length [x | x <- [1..20], n `mod` x /= 0] == 0

solve = head $ filter isDivisible $ iterate (\x -> x + productOfPrimes) (productOfPrimes)