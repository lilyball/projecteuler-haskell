-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
-- increases by 3330, is unusual in two ways: (i) each of the three terms are
-- prime, and, (ii) each of the 4-digit numbers are permutations of one another.
--
-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
-- exhibiting this property, but there is one other 4-digit increasing sequence.
--
-- What 12-digit number do you form by concatenating the three terms in this
-- sequence?
--

import Control.Monad
import Data.Numbers.Primes
import Data.List

primePermutations :: Int -> [Int]
primePermutations n = filter isPrime perms
  where perms = nub $ map read $ permutations (show n)

-- primePermutations >= the input, sorted
sortedCandidates :: Int -> [Int]
sortedCandidates n = sort $ filter (>= n) $ primePermutations n

-- finds all sequences of 3 numbers with the same delta
-- that start with the head
findSequences :: [Int] -> [[Int]]
findSequences (a:ns) = do
  b <- ns
  c <- dropWhile (<= b) ns
  guard $ b-a == c-b
  return [a,b,c]

primeSequences :: [[Int]]
primeSequences = do
  n <- takeWhile (<10000) $ dropWhile (<1000) primes
  findSequences $ sortedCandidates n

main = do
  let seq = head $ filter (not.(==1487).head) primeSequences
  putStrLn $ join $ map show seq
