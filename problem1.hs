module Main where

-- Problem 1:

-- If we list all the natural numbers below 10 that are multiples of 3
-- or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

-- Find the sum of all the multiples of 3 or 5 below 1000.

-- Solution:

-- Note that Sum(1,n) = n*(n+1)/2. The sum of all the multiples of k
-- between 1 and n, not including n, is

-- k + 2*k + 3k + ... + floor(n-1/k)*k = k * Sum(1,floor(n-1/k))

-- This allows us to define 'sumMultiples' below. When summing the
-- numbers that are multiples of 3 or 5, we need to subtract the
-- numbers that are multiples of 3*5 = 15, since otherwise we would
-- count them twice.

main :: IO ()
main = print $ sumThreeFive 1000

sumThreeFive n = sumMultiples 3 n + sumMultiples 5 n - sumMultiples 15 n

sumMultiples :: Int -> Int -> Int
sumMultiples k n = k * sumTo ((n-1) `div` k)
  where sumTo n = n * (n+1) `div` 2
  
