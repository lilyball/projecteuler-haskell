-- The following iterative sequence is defined for the set of positive integers:
--
-- n -> n/2 (n is even)
-- n -> 3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following sequence:
--
-- 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
--
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains
-- 10 terms. Although it has not been proved yet (Collatz Problem), it is thought
-- that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?
--
-- NOTE: Once the chain starts the terms are allowed to go above one million.
--

import Data.Function (on)
import Data.List (maximumBy)
import Data.Ord (comparing)

step :: Int -> Int
step n | even n    = n `div` 2
       | otherwise = 3*n + 1

collatz :: Int -> Int
collatz = collatz' 0
  where collatz' c 1 = succ c
        collatz' c n = collatz' (succ c) (step n)

main = do
  print $ fst $ maximumBy (comparing snd) [(n, collatz n) | n <- [1..999999]]
