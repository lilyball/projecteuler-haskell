{-
A collection of miscellaneous utilities used for project euler solutions
-}

module Utilities
    (
      primeFactors
    , divisors
    ) where

import qualified Sieve
import Data.List
import qualified Data.Set as Set

primeFactors :: Integer -> [Integer]
primeFactors n | n <= 0 = error "primeFactors: n must be >= 1"
primeFactors n =
    unfoldr aux (n, Sieve.primes)
  where aux :: (Integer, [Integer]) -> Maybe (Integer, (Integer, [Integer]))
        aux (1, _) = Nothing
        aux (n, pps@(p:ps))
            | n `mod` p == 0 = Just (p, (n `div` p, pps))
            | otherwise      = aux (n, ps)

divisors n = Set.toList $ divisors' $ primeFactors n
  where divisors' []     = Set.singleton 1
        divisors' (n:ns) = subdivs `Set.union` Set.map (n*) subdivs
          where subdivs = divisors' ns
