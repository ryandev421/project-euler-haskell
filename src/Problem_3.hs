module Problem_3
    ( solve
    ) where

isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

factorsOf xs = concat [[x,xs`div`x] | x <- [1..isqrt(xs)], xs `mod` x == 0]
isPrime xs = length (factorsOf xs) == 2

input = 600851475143
solve = maximum (filter isPrime (factorsOf input))