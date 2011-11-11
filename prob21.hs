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

import Data.Numbers.Primes
import Data.List (subsequences, nub)
import Data.Array
import Data.Ix
import Control.Monad (guard)

import Test.QuickCheck

divisors :: (Num a, Integral a) => a -> [a]
divisors = map product . nub . subsequences . primeFactors

properDivisors :: (Num a, Integral a) => a -> [a]
properDivisors = init . divisors

sumOfDivisors :: (Num a, Integral a) => a -> a
sumOfDivisors = sum . properDivisors

-----------------

{-divisorAry :: Array Int Int
divisorAry = array (1,1000) [(i,d) | i <- [1..1000], let d = sumOfDivisors i]

amicablePairs :: [(Int,Int)]
amicablePairs = do
  (i,a) <- assocs divisorAry
  guard $ a > i -- always produce a sorted pair
  guard $ bounds divisorAry `inRange` a -- is the pair in range?
  guard $ divisorAry ! a == i -- is it amicable?
  return (i,a)

amicableNumbers :: [Int]
amicableNumbers = foldr step [] amicablePairs
  where step a b = fst a : snd a : b-}

amicableNumbers :: (Int,Int) -> [Int]
amicableNumbers bounds = do
  i <- uncurry enumFromTo bounds
  let d = sumOfDivisors i
  guard $ d > i -- only return a sorted pair
  guard $ sumOfDivisors d == i
  if inRange bounds d then [i,d] else [i]

main = do
  print $ sum $ amicableNumbers (1,10000)

-----------------

testDivisors = quickCheck test
  where test :: Int -> Bool
        test n = all prop pairs
          where prop = (==n) . uncurry (*)
                pairs = zip (reverse divs) divs
                divs = divisors n
