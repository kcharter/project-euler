module Main where

-- Problem 2

-- Each new term in the Fibonacci sequence is generated by adding the
-- previous two terms. By starting with 1 and 2, the first 10 terms
-- will be:

--   1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

-- By considering the terms in the Fibonacci sequence whose values do
-- not exceed four million, find the sum of the even-valued terms.

-- Solution: well, here's the brute force solution. Actually, it's not
-- that brute force. We exceed four million within the first 40 terms
-- in the Fibonacci sequence, so this is very quick.

main :: IO ()
main = print $ sum $ filter even $ takeWhile (<=4000000) $ fibs


fibs :: [Integer]
fibs = 1:2:fibs' 1 2
fibs' pred1 pred = let f = pred1 + pred
                   in f:fibs' pred f