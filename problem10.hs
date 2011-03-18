module Main where

{-

Problem 10:

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.

-}

{-

Solution:

Don't expand the seive to the primes up to 2000000; instead, do primality tests.

-}

import Primality

-- This is still not terribly fast, though the compiled version takes
-- less than 4 seconds on my laptop. I wonder if there is a faster
-- way than simply finding primes and adding them.

sumPrimesLessThan :: Integer -> Integer
sumPrimesLessThan n =
  sum $ takeWhile (<n) $ filter isPrime [2..]

main :: IO ()
main = print $ sumPrimesLessThan 2000000
