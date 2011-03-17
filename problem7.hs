module Main where

import Sieve

-- Problem 7:

-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we
-- can see that the 6th prime is 13.

-- What is the 10001st prime number?

-- Solution:

-- The easy, but slow solution to this problem is below. I ran this
-- and submitted the answer, though it took several seconds to run and
-- consumed quite a bit of memory.

easyButSlow :: Integer
easyButSlow = head $ drop 10000 sieve

-- The faster way to do this might be to do primality tests where we
-- don't bother caching all the results, like we do when we evaluate a
-- prefix of the list of all primes using the sieve. A number n is
-- prime iff it is indivisible by every prime number p where p <= the
-- square root of n, i.e. p^2 <= n.

-- In 'isPrime' below, this greatly reduces how many elements of
-- 'sieve' we evaluate. Note, however, that this still isn't a good
-- primality tester for huge numbers. For those, we'd probably want a
-- solution that puts a hard limit on the size of a set of cached
-- primes (a prefix of 'sieve') and otherwise uses itself to find the
-- remaining primes below the square root.
              

isPrime :: Integer -> Bool
isPrime n = isPrime' sieve
  where isPrime' (p:rest) = 
          case compare (p*p) n of
            LT -> n `mod` p /= 0 && isPrime' rest
            EQ -> False
            GT -> True

-- Indeed, this is a *lot* faster, and consumes a lot less memory.
            
fasterIHope :: Integer
-- Minor optimization: look for the 10000th odd prime, since
-- 2 is the only even prime.
fasterIHope = head $ drop 9999 $ filter isPrime odds
  where odds = map (\n -> 2*n+1) [1..]
        
main :: IO ()
main = print fasterIHope
