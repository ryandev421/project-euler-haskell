module Problem_2
    ( solve
    ) where

fibo a b = a:fibo b (a+b)
solve = sum (filter even (takeWhile (<4000000) (fibo 1 2)))