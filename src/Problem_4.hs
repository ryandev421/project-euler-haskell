module Problem_4
    ( solve
    ) where

reverseInt :: Integer -> Integer
reverseInt x = read . reverse . show $ x

isPalindrome n = n == (reverseInt n)

solve = maximum $ filter isPalindrome [x*y | x <-[100..999], y<-[100..999]]