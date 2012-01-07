-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
-- also prime.
--
-- What is the largest n-digit pandigital prime that exists?
--

import Data.Numbers.Primes
import Data.List (permutations)
import Data.Char (intToDigit)

main = do
    print $ maximum $ filter isPrime list
  -- 9-digit pandigitals cannot be prime as sum [1..9]=45 is divisible by 3
  -- 8-digit pandigitals cannot be prime as sum [1..8]=36 is divisible by 3
  where list = do
          n <- [1..7]
          ps <- permutations [1..n]
          return $ read $ map intToDigit ps
