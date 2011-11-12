module Euler.Divisors (
  divisors, properDivisors
  ) where

import Data.Numbers.Primes
import Data.List (nub, subsequences)

divisors :: (Num a, Integral a) => a -> [a]
divisors = map product . nub . subsequences . primeFactors

properDivisors :: (Num a, Integral a) => a -> [a]
properDivisors = init . divisors
