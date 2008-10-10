{-
http://projecteuler.net/index.php?section=problems&id=14

The following iterative sequence is defined for the set of positive integers:

        n -> n/2 (n is even)
        n -> 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

        13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1

It can be seen that this sequence (starting at 13 and finishing at 1)
contains 10 terms. Although it has not been proved yet (Collatz Problem),
it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
-}

-- this program doesn't run under runghc - it hits a stack overflow
-- but it works fine when compiled

import Data.Array
import Control.Arrow ((&&&))
import Data.Ord (comparing)
import Data.List (maximumBy)

limit = 999999

memo = array bounds [(i,chain' i) | i <- range bounds]
  where bounds = (1, limit)

chain :: Integer -> Integer
chain n
    | inRange (bounds memo) n = memo ! n
    | otherwise = chain' n

chain' :: Integer -> Integer
chain' 1 = 1
chain' n
    | even n    = let n' = n `div` 2 in succ (chain n')
    | otherwise = let n' = 3*n+1 in succ (chain n')

main = do
  let chains = map (id &&& chain) [1..limit]
  print $ fst $ maximumBy (comparing snd) $ chains
