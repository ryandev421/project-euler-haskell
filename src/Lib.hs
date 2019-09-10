module Lib
    ( isPrime, factorsOf
    ) where

isqrt :: (Integral a) => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

factorsOf :: (Integral a) => a -> [a]
factorsOf n = 
    [x | x <- [1..isqrt_num], n `mod` x == 0] ++ [n `div` x | x <- [1..(isqrt_num-1)], n `mod` x == 0]
    where isqrt_num = isqrt(n)

isPrime :: (Integral a) => a -> Bool
isPrime n = length (factorsOf n) == 2