module Problem_12
    ( solve
    ) where

import qualified Lib

triangle_numbers :: (Integral a) => [a]
triangle_numbers = [x * (x+1) `div` 2 | x <- [1..]]

solve = head $ dropWhile (\x -> length (Lib.factorsOf x) <= 500) $ triangle_numbers