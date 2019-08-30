module Problem_10
    ( solve
    ) where

import qualified Lib

limit = 2000000
solve = sum $ filter Lib.isPrime $ [2..(limit - 1)]
