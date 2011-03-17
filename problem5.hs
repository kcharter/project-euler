module Main where

-- Problem 5:

-- 2520 is the smallest number that can be divided by each of the
-- numbers from 1 to 10 without any remainder.

-- What is the smallest positive number that is evenly divisible by
-- all of the numbers from 1 to 20?

-- Solution:

-- We'll use some facts about primes here.

import Sieve

-- x is divisible by y iff x is divisible by all the prime factors of
-- y. Therefore, if x is divisible by every y from 1 to n inclusive,
-- then

-- - x must be divisible by every prime factor of every number between
--   1 and n

-- - for each such prime factor p, x must be divisible by p ^
--   maxExponent(p,n), where maxExponent(p,n) is the largest exponent
--   of p in the prime factorization of any number less than n.

-- Now, the prime factors of numbers between 1 and n are all
-- themselves numbers between 1 and n. Further, maxExponent(p,n) is
-- the largest power of p that is less than or equal to n.

-- Therefore, the very smallest number that is divisible by all the
-- numbers 1 through n is the product of p ^ maxExponent(p,n) for
-- all the primes p <= n.

maxExponent :: Integer -> Integer -> Integer
maxExponent p n = maxExponent' 1 p
  where maxExponent' e sofar | sofar > n = e-1
                             | otherwise = maxExponent' (e+1) (sofar*p)

maxPrimeExponentsTo :: Integer -> [(Integer, Integer)]
maxPrimeExponentsTo n =
  map (\p -> (p, maxExponent p n)) $ takeWhile (<= n) sieve

smallestDivisibleByOneTo =
  product . map (uncurry (^)) . maxPrimeExponentsTo
  
main :: IO ()
main = print $ smallestDivisibleByOneTo 20
