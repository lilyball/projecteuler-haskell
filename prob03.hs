-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143 ?
--

-- The following is a trivial solution
{-import Data.Numbers.Primes

main = do
  print $ last $  primeFactors 600851475143-}

-- Here's one that uses trial division
main = do
  print $ last $ primeFactors 600851475143

primeFactors :: (Integral n) => n -> [n]
primeFactors n = primeFactors' 2 n
  where primeFactors' base n
          | n <= 1            = []
          | n `mod` base == 0 = base : primeFactors' base (n `div` base)
          | otherwise         = primeFactors' (base+1) n
