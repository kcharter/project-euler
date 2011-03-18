module Primality where

import Sieve (sieve)

-- | Tests a number for primality by checking whether it is
-- indivisible by every prime less than or equal to its square root.
--
-- The primes up to the square root are obtained using the sieve of
-- Eratosthenes, so this isn't a good way to check the primality of
-- really huge numbers.

isPrime :: Integer -> Bool
isPrime n = isPrime' sieve
  where isPrime' (p:rest) = 
          case compare (p*p) n of
            LT -> n `mod` p /= 0 && isPrime' rest
            EQ -> False
            GT -> True
