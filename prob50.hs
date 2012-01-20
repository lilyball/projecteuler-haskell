-- The prime 41, can be written as the sum of six consecutive primes:
--
--                        41 = 2 + 3 + 5 + 7 + 11 + 13
--
-- This is the longest sum of consecutive primes that adds to a prime below
-- one-hundred.
--
-- The longest sum of consecutive primes below one-thousand that adds to a
-- prime, contains 21 terms, and is equal to 953.
--
-- Which prime, below one-million, can be written as the sum of the most
-- consecutive primes?
--

import Data.List
import Data.Maybe
import Data.Numbers.Primes
import Data.Ord (comparing)

maxSum = 1000000

sequences :: [[Int]]
sequences = mapMaybe findSeq $ tails $ takeWhile (<maxSum) primes
  where findSeq :: [Int] -> Maybe [Int]
        findSeq ps = let sums = scanl1 (+) ps
                         pairs = zip [1..] $ takeWhile (<maxSum) sums
                     in fmap (flip take ps . fst) $ listToMaybe $ filter (isPrime.snd) $ reverse pairs

longestSequence :: [Int]
longestSequence = maximumBy (comparing length) sequences

main = do
  print $ sum longestSequence
