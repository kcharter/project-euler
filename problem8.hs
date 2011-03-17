module Main where

{-

Problem 8:

Find the greatest product of five consecutive digits in the 1000-digit number below.

73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450

-}

{-

Solution:

Note that for a list of numbers of length n, we can do this problem
with O(n) multiplications and divisions. Here is a sketch of the idea.

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

import Data.List (foldl')

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

-- 'mcp k l' is the maximum product of 'k' consecutive numbers in 'l',
-- implemented as sketched above
mcp :: (Integral a) => Int -> [a] -> Maybe a
mcp k l =
  let firstk = take k l
      p = product firstk
  in if length firstk < k
     then Nothing
     else Just $ mcp' p (fromList firstk) p (drop k l)
       where mcp' maxSofar _ _ [] = maxSofar
             mcp' maxSofar lastFactors last (n:rest) =
               mcp' maxSofar' lastFactors'' last' rest
                 where maxSofar' = max maxSofar last'
                       (f, lastFactors') = dequeue' lastFactors
                       lastFactors'' = enqueue n lastFactors'
                       last' = if f /= 0
                               then (last `div` f) * n
                               else product $ toList lastFactors''

-- we obtain a list of integers from the digits by reading each
-- character of the string version as a singleton string.
theList :: [Int]
theList = map (read . (:[])) asString

-- here is the original input as a string of digits
asString =
  concat ["73167176531330624919225119674426574742355349194934",
          "96983520312774506326239578318016984801869478851843",
          "85861560789112949495459501737958331952853208805511",
          "12540698747158523863050715693290963295227443043557",
          "66896648950445244523161731856403098711121722383113",
          "62229893423380308135336276614282806444486645238749",
          "30358907296290491560440772390713810515859307960866",
          "70172427121883998797908792274921901699720888093776",
          "65727333001053367881220235421809751254540594752243",
          "52584907711670556013604839586446706324415722155397",
          "53697817977846174064955149290862569321978468622482",
          "83972241375657056057490261407972968652414535100474",
          "82166370484403199890008895243450658541227588666881",
          "16427171479924442928230863465674813919123162824586",
          "17866458359124566529476545682848912883142607690042",
          "24219022671055626321111109370544217506941658960408",
          "07198403850962455444362981230987879927244284909188",
          "84580156166097919133875499200524063689912560717606",
          "05886116467109405077541002256983155200055935729725",
          "71636269561882670428252483600823257530420752963450"]

main :: IO ()
main = print $ mcp 5 theList

