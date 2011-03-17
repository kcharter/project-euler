module Sieve where

sieve :: [Integer]
sieve = sieve' [2..]
  where sieve' (n:rest) =
          n:sieve' (filter (not . multiple n) rest)
        multiple n m = m `mod` n == 0
