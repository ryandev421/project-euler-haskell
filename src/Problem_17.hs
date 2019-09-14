module Problem_17
    ( solve
    ) where

import qualified Lib

length_under_20 :: Int -> Int
length_under_20 x = [0, 3, 3, 5, 4, 4, 3, 5, 5, 4, 3, 6, 6, 8, 8, 7, 7, 9, 8, 8] !! x

length_tens :: Int -> Int
length_tens x = [0, 0, 6, 6, 5, 5, 5, 7, 6, 6] !! x

length_number :: Int -> Int
length_number n =
    if n >= 100
        then length_number (div n 100) + 7 + length_number (mod n 100) + (if mod n 100 == 0 then 0 else 3)
    else
        if n < 20
            then length_under_20 n
        else 
            length_tens (div n 10) + length_under_20 (mod n 10)

solve = 11 + (sum $ map length_number [1..999])