module Main where

-- Problem 3:

-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

-- Solution

-- First, we'll use the sieve of Eratosthenes to generate the list of primes.

import Sieve
        
-- To find the maximum prime divisor, we divide by larger and larger
-- prime divisors, until we encounter a prime that is larger than the
-- result of the division. Note that the sieve is already sorted in
-- ascending order. At first, I didn't bother doing the division by
-- the candidate, but it *greatly* slows the search.
        
largestPrimeDivisor n = largestDivisor n 1 (head sieve) (tail sieve)


largestDivisor n last candidate cs@(next:rest) =
  if candidate > n
  then last
  else if n `mod` candidate == 0
       then largestDivisor (n `div` candidate) candidate candidate cs
       else largestDivisor n last next rest
  
main :: IO ()
main = print $ largestPrimeDivisor 600851475143