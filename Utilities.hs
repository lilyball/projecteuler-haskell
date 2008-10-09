{-
A collection of miscellaneous utilities used for project euler solutions
-}

module Utilities
    (
      primeFactors
    ) where

import qualified Sieve
import Data.List

primeFactors :: Integer -> [Integer]
primeFactors n =
    unfoldr aux (n, Sieve.primes)
  where aux :: (Integer, [Integer]) -> Maybe (Integer, (Integer, [Integer]))
        aux (1, _) = Nothing
        aux (n, pps@(p:ps))
            | n `mod` p == 0 = Just (p, (n `div` p, pps))
            | otherwise      = aux (n, ps)
