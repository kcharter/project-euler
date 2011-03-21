{-|

A utility for finding the maximum product of 'k' consecutive integers
in a list of integers. This module was originally the bulk of the
solution to Problem 8.

-}

module MPC (mpc) where

import Data.List (foldl')

{-

For a list of numbers of length n, we can do this problem with O(n)
multiplications and divisions. Here is a sketch of the idea.

We scan the list from left to right maintaining these pieces of state:

- the maximum product seen so far

- the five numbers comprising the factors of the most recently computed product

- the most recently computed product

When the remainder of the list is empty, we're done, and the result is
the maximum we have seen so far.

Otherwise,

- let A be the head of the remainder of the list

- let F be the first element of L, the list of the factors of the last product

- let P be the value of the last product

We revise the state as follows.

- compute the new list of factors L' by dropping F and appending A

- compute the new product P' = (P / F) * A, provided F is not zero.
  Otherwise, we need to compute P' from L'

- drop A from the list of remaining numbers

- if P' is greater than the maximum seen so far, it becomes the
  maximum seen so far

Note that conceptually the list L of most recent factors is a queue (a
FIFO list). In Haskell, we may want to use a functional queue for this
rather than a simple list, in order to limit the overhead cost of
concatenation.

-}

newtype Queue a = Queue ([a],[a]) deriving Show

empty :: Queue a
empty = Queue ([],[])

fromList :: [a] -> Queue a
fromList = foldl' (flip enqueue) empty

toList :: Queue a -> [a]
toList (Queue (front, rback)) = front ++ reverse rback

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue (front, rback)) = Queue (front, x:rback)

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue (x:front, rback)) = Just (x, Queue (front, rback))
dequeue (Queue ([], rback)) =
  case reverse rback of
    x:front -> Just (x, Queue (front, []))
    [] -> Nothing
    
dequeue' :: Queue a -> (a, Queue a)    
dequeue' = maybe (error "Queue is empty") id . dequeue

-- 'mpc k l' is the maximum product of 'k' consecutive numbers in 'l',
-- implemented as sketched above
mpc :: (Integral a) => Int -> [a] -> Maybe a
mpc k l =
  let firstk = take k l
      p = product firstk
  in if length firstk < k
     then Nothing
     else Just $ mpc' p (fromList firstk) p (drop k l)
       where mpc' maxSofar _ _ [] = maxSofar
             mpc' maxSofar lastFactors last (n:rest) =
               mpc' maxSofar' lastFactors'' last' rest
                 where maxSofar' = max maxSofar last'
                       (f, lastFactors') = dequeue' lastFactors
                       lastFactors'' = enqueue n lastFactors'
                       last' = if f /= 0
                               then (last `div` f) * n
                               else product $ toList lastFactors''



