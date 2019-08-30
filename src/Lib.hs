module Lib
    ( isPrime
    ) where

isqrt :: (Integral a) => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

isPrime :: (Integral a) => a -> Bool
isPrime n = n > 1 && length ([() | k <- [2..(isqrt n)], n `mod` k == 0]) == 0