module Main where

{-

Problem 9:

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a2 + b2 = c2

For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

-}

{-

Solution:

How about trying to cut down on the search space with a little algebra
first?

Our equations are

 a^2 + b^2 = c^2   (1)
 a + b + c = 1000  (2)

By (2)

  c = 1000 - (a + b)

and substituting in (1) we get

 a^2 + b^2 = (1000 - (a + b))^2
           = 1000000 - 2000 (a + b) + a^2 + 2ab + b^2

and cancelling the common terms we get

 0 = 1000000 - 2000 (a + b) + 2ab

We can therefore write 'b' in terms of 'a'

 b (2000 - 2a) = 1000000 - 2000 a

or

 b = 1000000 - 2000 a = 500000 - 1000 a = 1000 *  (500 - a)     (3)
     ----------------   ---------------          ----------
      2000 - 2 a          1000 - a                1000 - a

Now, 'a', 'b' and 'c' must all be positive integers, and by (2) each
of them must be less than 1000. However, if

  a < 1000

then the denominator in equation (3) is positive, so for 'b' to be
positive the numerator must likewise be positive. This implies

  a < 500    (4)

Further, since

  b < 1000

we must have

  (500 - a) / (1000 - a)  <  1

Moreover, 'b' will be an integer iff (500 - a) / (1000 - a) when
reduced to least terms has a denominator that divides 1000 = 2^3 5^3.

So, when searching for 'a' we want to find an integer between 1 and
499 such that

 1000 mod ((1000 - a) / gcd (500 - a) (1000 -a)) = 0

-}

candidateAs :: [Int]
candidateAs = filter candidateA [1..499]
  where candidateA a = 1000 `mod` (a_1000 `div` gcd a_500 a_1000) == 0
          where a_1000 = 1000 - a
                a_500 = 500 - a

candidateTriples :: [(Int,Int,Int)]
candidateTriples = map candidateTriple candidateAs
  where candidateTriple a =
          (a, b, candidateC a b)
            where b = candidateB a
        candidateB a = (1000 * (500 - a)) `div` (1000 - a)
        candidateC a b = 1000 - a - b
        
verify :: (Int,Int,Int) -> Bool
verify (a,b,c) =
  0 < a && a < b && b < c &&
  a^2 + b^2 == c^2 &&
  a + b + c == 1000
  
main :: IO ()
main = print $ prod $ head $ candidateTriples
  where prod (a,b,c) = a*b*c
        