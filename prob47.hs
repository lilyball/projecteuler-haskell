-- The first two consecutive numbers to have two distinct prime factors are:
--
--      14 = 2 x 7
--      15 = 3 x 5
--
-- The first three consecutive numbers to have three distinct prime factors are:
--
--      644 = 2² x 7 x 23
--      645 = 3 x 5 x 43
--      646 = 2 x 17 x 19.
--
-- Find the first four consecutive integers to have four distinct primes
-- factors. What is the first of these numbers?
--

import Data.Numbers.Primes
import Data.List (tails, find, union, nub)

factors :: [[Int]]
factors = map primeFactors [1..]

main :: IO ()
main = do
  let n = 4 -- number of factors to go for
      nums = zip [2..] $ map (take n) $ tails $ map (nub.primeFactors) [2..]
      -- nums is now a list of (n,[fac n, fac n+1, fac n+2, fac n+3])
      hasCount = all ((==n).length) . snd
  let nums' = filter hasCount nums
      -- nums' only contains sequences with the right number of factors
  print $ head $ nums'


