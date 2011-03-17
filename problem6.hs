module Main where

-- Problem 6:

-- The sum of the squares of the first ten natural numbers is,

--   1^2 + 2^2 + ... + 10^2 = 385

-- The square of the sum of the first ten natural numbers is,

--   (1 + 2 + ... + 10)^2 = 552 = 3025

-- Hence the difference between the sum of the squares of the first
-- ten natural numbers and the square of the sum is 3025 − 385 = 2640.

-- Find the difference between the sum of the squares of the first one
-- hundred natural numbers and the square of the sum.

{-

Solution:

Let sumsq n = 1^2 + ... + n^2

I haven't been able to find a closed-form expression for this, like
there is for (sum n). However, there is a nice recurrence relation for

  (sum n)^2 - (sumsq n)

First note the obvious recurrences

  sum 1 = 1
  sum n = n + sum (n-1), n > 1

and

  sumsq 1 = 1  
  sumsq n = n^2 + sumsq (n-1), n > 1

So let

  d n = (sum n)^2 - sumsq n

Therefore we see that

  d 1 = (sum 1)^2 - sumsq 1 = 0

and for n > 1 we have

  d n
    = (sum n)^2 - sumsq n
    = (n + sum (n-1))^2 - (n^2 + sumsq (n-1))
    = n^2 + 2n(sum (n-1)) + (sum (n-1))^2 - n^2 - sumsq (n-1)
    = 2n(sum (n-1)) + (sum (n-1))^2 - sumsq (n-1)
    = 2n(sum (n-1)) + d (n-1)

Now, we know that

  sum m = m (m + 1) / 2

so

  sum (n-1) = (n-1) ((n-1) + 1) / 2 = n (n - 1) / 2

and do we get

  d n = 2n(sum (n-1)) + d (n-1)
      = 2n(n(n-1)/2) + d (n-1)
      = n^2 (n-1) + d (n-1)

With this recurrence, we can compute (d n) using n-1 additions, 
n-1 subtractions, and 2(n-1) multiplications. For (d 100), this
is isn't much work at all.

-}

-- Here is a tail-recursive version.

d :: Integer -> Integer
d 1 = 0
d n = d' 1 0
  where d' m last | m > n = last
                  | otherwise = d' (m+1) (m*m*(m-1) + last)

-- Here is a simpler version that uses a non-tail call. Because of
-- lazy evaluation, my intuition about which is better isn't that well
-- formed. This version has the benefit of being much easier to read.

dr :: Integer -> Integer
dr 1 = 0
dr n = n*n*(n-1) + d (n-1)

main :: IO ()
main = print $ d 100

