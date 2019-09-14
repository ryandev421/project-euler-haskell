module Problem_16
    ( solve
    ) where

import qualified Lib

digits :: Integer -> [Integer]
digits 0 = []
digits n = [mod n 10] ++ digits (div n 10)

solve = sum $ digits (2^1000)