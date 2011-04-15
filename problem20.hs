module Main where

{-

Problem 20

n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!

-}

{-

Solution

Again, I just decided to brute force this one. It's near-instantaneous
in ghci. Still, as with problem 16, I have the feeling that there is
way to do this that doesn't require computing 100! in order to sum its
digits, and that doesn't require unbounded integers.

-}

main :: IO ()
main = print $ sum $ map (read . (:[])) $ show $ product [1..100]