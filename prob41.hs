-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
-- also prime.
--
-- What is the largest n-digit pandigital prime that exists?
--

import Data.Numbers.Primes
import Data.List ((\\))

isPandigital :: Int -> Bool
isPandigital d = ds \\ take n (enumFrom '1') == []
  where ds = show d
        n = length ds

main = do
  -- 9 digit pandigitals cannot be prime as sum [1..9] is divisible by 3
  -- 8 digit pandigitals cannot be prime as sum [1..8] is divisible by 3
  -- Limit all primes to those 7 digits and fewer
  print $ maximum $ filter isPandigital $ takeWhile (<=7654321) primes
