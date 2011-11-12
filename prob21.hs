{-# OPTIONS_GHC -Wall #-}
-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n
-- which divide evenly into n).
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and
-- each of a and b are called amicable numbers.
--
-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44,
-- 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4,
-- 71 and 142; so d(284) = 220.
--
-- Evaluate the sum of all the amicable numbers under 10000.
--

import Data.Ix (inRange)
import Control.Monad (guard)

import Euler.Divisors

sumOfDivisors :: (Num a, Integral a) => a -> a
sumOfDivisors = sum . properDivisors

amicableNumbers :: (Int,Int) -> [Int]
amicableNumbers bounds = do
  i <- uncurry enumFromTo bounds
  let d = sumOfDivisors i
  guard $ d > i -- only return a sorted pair
  guard $ sumOfDivisors d == i
  if inRange bounds d then [i,d] else [i]

main :: IO ()
main = do
  print $ sum $ amicableNumbers (1,10000)
