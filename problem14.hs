{-# LANGUAGE TupleSections #-}

module Main where

{-

Problem 14:

The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:
13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1)
contains 10 terms. Although it has not been proved yet (Collatz
Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.

-}

{-

Solution:

An obvious way to compute this problem is to brute force it: for each
starting number in [1..999999], compute the sequence, compute its
length, and maintain a running maximum length. However, if the
sequences could get very long, this could take a really long time. 

Now, if all of the known starting numbers generate sequences that
reach 1, then this means that all sequences eventually reach a power
of 2. I'm going to guess that there numbers other than powers of 2
that are 'attractors', so that memoization can help speed up the
search for the biggest number. -}

import Prelude hiding (sequence)
import qualified Data.IntMap as DIM
import Data.List (foldl')

next :: Int -> Int
next n | even n = n `div` 2
       | otherwise = 3*n + 1

-- These functions compute the sequence and its length without any kind of memoization.

sequence :: Int -> [Int]
sequence n = n:after n
  where after 1 = []
        after n = let n' = next n in n':after n'

sequenceLength :: Int -> Int
sequenceLength = length . sequence

-- Even computing the sequences up to n == 1000, you notice the time
-- increasing quite quickly. You also notice that a lot of sequence
-- suffixes are repeated, so memoization is probably going to pay off
-- a lot.
        
bruteForce :: Int -> Int
bruteForce n = maximum $ map sequenceLength [1..n]

type Memo = DIM.IntMap Int

memoLength :: Memo -> Int -> (Memo, Int)
memoLength memo n =
  maybe (memoLength' memo n) (memo,) (DIM.lookup n memo)
    where memoLength' memo 1 = (DIM.insert 1 1 memo, 1)
          memoLength' memo n = result `seq` result
            where result = (DIM.insert n l memo' , l `seq` l)
                  l = 1 + l'
                  (memo', l') = memoLength memo (next n)

-- This version computes the maximum sequence length for starting
-- sequence numbers from 1 up to and including n.  It uses memoization
-- to avoid repeated work. Informal testing shows it's quite a bit
-- faster at n = 100000, so it should be a really big improvement for
-- n = 999999.
                  
-- The original version of maxLength used used 'foldr', flipped the
-- arguments for "maxLength'" and called 'reverse [1..n]'. This leads
-- to a stack overflow in the compiled program, and makes the ghci
-- version run very slowly. At first I thought the problem was that I
-- was building long chains of unevaluated 'max', and '+' calls and
-- pair constructors, so I used 'seq' to add strictness. However, I
-- couldn't get rid of the stack overflow until I stopped trying to do
-- 'reverse [1..n]'. That's a good lesson: avoid reversing really long
-- lists, particularly if you need to pass over them only once.
                  
-- The compiled version of this code with -O2 finds the answer in a
-- little more than 6 seconds.
                  
withMaxLength :: Int -> (Int, Int)
withMaxLength n =
  snd $ foldl' maxLength' (DIM.empty, (1,1)) [1..n]
    where maxLength' (memo, sofar@(_, length)) m = result `seq` result
            where result = sofar' `seq` (memo', sofar')
                  sofar' = if l > length then (m,l) else sofar
                  (memo', l) = memoLength memo m
                  
main :: IO ()
main = print $ withMaxLength 999999
