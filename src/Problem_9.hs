module Problem_9
    ( solve
    ) where

sumNumbers = 1000

solve = head 
    $ map (\(a, b, c) -> a*b*c) 
    $ filter (\(a, b, c) -> (a + b > c && a^2 + b^2 == c^2)) 
    $ map (\(a, b) -> (a, b, sumNumbers - (a + b))) 
    $ [(a, b) | a <- [1..sumNumbers], b <- [a..sumNumbers]]