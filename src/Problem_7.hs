module Problem_7
    ( solve
    ) where

primes = func [2..] where func (prime:xs) = prime : func [x | x <- xs, x `mod` prime /=0]

solve = last $ take 10001 $ primes
