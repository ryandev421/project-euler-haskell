module Problem_14
    ( solve
    ) where

import qualified Lib

length_collatz :: Integer -> Int
length_collatz 1 = 1
length_collatz n = 1 + length_collatz (if mod n 2 == 0 then (div n 2) else (3 * n + 1))

findMaxIndex v [] = v
findMaxIndex v (x:xs) = if (snd x > snd v) then (findMaxIndex x xs) else (findMaxIndex v xs)

solve = fst $ findMaxIndex (0,0) $ map (\x -> (x, length_collatz x)) [1..10^6]