module Problem_6
    ( solve
    ) where

sumOfSquares = sum [x^2 | x <- [1..100]]
squareOfSum = (sum [x | x <- [1..100]]) ^ 2

solve= squareOfSum - sumOfSquares