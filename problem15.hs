module Main where

{-

Problem 15:

Starting in the top left corner of a 2×2 grid, there are 6 routes
(without backtracking) to the bottom right corner.

How many routes are there through a 20×20 grid?

-}

{-

Solution:

In order to move from the top-left to the bottom-right corner of an
n-by-n grid without loops or back-tracking, we need to move n steps to
the left and n steps down. The different routes arise because we can
choose to take the steps in different orders.

There are 2n steps in total. n of them must be down. There are

 2n choose n = (2n)! / n! (2n - n)! = (2n)! / n! n!

ways to distribute the n down steps over the 2n total steps.

Now, using unlimited precision integers, we could compute the number
of paths. But suppose we didn't have those. The trick would be to
compute this without having to compute the factorials.

Note   (2n)! / n! n! = (2n * ... * n * ... * 1)
                       ------------------------
                         n! (n * ... * 1)

                     = (2n * ... * (n+1))
                       ------------------
                        n * ... * 1

We can probably accumulate the ratio one term at a time, alternating
multiplications in the numerator and denominator, and reducing to
least terms after each multiplication. In Haskell we could do this
using rational numbers from the libraries. However, I'll implement it
here just to see that I've got the details right.

-}

newtype Rat a = Rat (a, a)

make :: (Integral a) => a -> a -> Rat a
make a b = Rat (a `div` g, b `div` g)
  where g = gcd a b
        
rmul :: (Integral a) => Rat a -> Rat a -> Rat a
rmul r s = make (num r * num s) (denom r * denom s)
        
num :: Rat a -> a        
num (Rat (a, _)) = a

denom :: Rat a -> a
denom (Rat (_, b)) = b

-- For the problem, where n = 20, you need 64-bit integers to
-- compute the answer, even with the reductions to least terms.

tlbrPaths :: Int -> Int
tlbrPaths n | n <= 0 = 0
            | otherwise = num $ foldr rmul (make 1 1) rats
              where rats = map mkrat [1..n]
                    -- the rats are the ratios
                    -- (n+1,1), (n+2,2), ..., (2n,n)
                    mkrat m = make (m+n) m
                    
main :: IO ()
main = print $ tlbrPaths 20
