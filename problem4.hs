module Main where

-- Problem 4:

-- A palindromic number reads the same both ways. The largest
-- palindrome made from the product of two 2-digit numbers is 9009 =
-- 91 × 99.

-- Find the largest palindrome made from the product of two 3-digit
-- numbers.

-- Solution:

-- First, a few observations.

-- There are 900 three-digit numbers between

--  100 = 10^2   and   999 = 10^3 - 1

-- so the products of three-digit numbers are between

--  10^4 = 10000   and
--  (10^3 - 1)^2 = 10^6 - 2(10^3) + 1 = 998001

-- The number of unique products is no more than (900^2)/2.

-- Now, every palindromic number between 10000 and 998001 has either
-- five digits or six digits. The first digit of a palindrome cannot
-- be zero, so the number of five-digit palindromes is

--  9 * 10 * 10 = 900

-- and the number of six-digit palindromes is also

--  9 * 10 * 10 = 900

-- Therefore, the combined set five-digit and six-digit palindromic
-- numbers has only 1800 elements. If we enumerate this set in
-- descending order, and can efficiently test whether a number is a
-- product of two three-digit numbers, then we can quickly find the
-- answer to the problem.

-- A simpler alternative is to generate the list of unique products in
-- descending order, and test for palindromicity.

-- when is

-- n * (n - k) < (n - d) * (n - d)
-- ==> n^2 - k * n < n^2 - 2 * d * n + d^2
-- ==> (2*d - k) * n < d^2
-- ==> n < d^2 / (2*d - k)

-- TODO: the result of 'products' below is *not* in fact sorted in
-- descending order; can we generate it without sorting it?

-- Note that we can solve the problem without sorting, but this means
-- we must search for the maximum and thereby examine all the
-- palindromes. This takes only a second or two even in ghci, but I'm
-- sure we can do better.

products :: Integer -> Integer -> [Integer]
products min max = products' max max
  where products' n nk | n == min = [n*nk]
                       | nk == min = (n*nk):products' (n-1) (n-1)
                       | otherwise = (n*nk):products' n (nk-1)

palindrome :: Integer -> Bool
palindrome n = let s = show n in reverse s == s

palindromes :: Integer -> Integer -> [Integer]
palindromes min max = filter palindrome (products min max)

main :: IO ()
main = print $ maximum $ palindromes 100 999
