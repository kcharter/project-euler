module Main where

{-

Problem 16

2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?

-}

{-

Solution: At first I thought that there was a clever way to compute
the digits without actually doing the exponentiation. But it turns out
that if you have arbitrary precision integers, it actually takes
almost no time at all to compute 2^1000. Then, converting the number
to a string (base 10) and summing the digits is trivial. This is
almost instantaneous even without compiling it.

-}

main :: IO ()
main = print $ sum $ map (read . (:[])) $ show $ 2^1000
