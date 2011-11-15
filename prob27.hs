-- Euler published the remarkable quadratic formula:
--
--                                  n² + n + 41
--
-- It turns out that the formula will produce 40 primes for the consecutive
-- values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is
-- divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly
-- divisible by 41.
--
-- Using computers, the incredible formula  n²  79n + 1601 was discovered, which
-- produces 80 primes for the consecutive values n = 0 to 79. The product of the
-- coefficients, 79 and 1601, is 126479.
--
-- Considering quadratics of the form:
--
--     n² + an + b, where |a| < 1000 and |b| < 1000
--
--                                  where |n| is the modulus/absolute value of n
--                                                    e.g. |11| = 11 and |4| = 4
--
-- Find the product of the coefficients, a and b, for the quadratic expression
-- that produces the maximum number of primes for consecutive values of n,
-- starting with n = 0.
--

import Data.Numbers.Primes
import Data.List (maximumBy)
import Data.Ord (comparing)
import Text.Printf

main = do
    let candidates = do
          a <- [1..999] :: [Int]
          b <- takeWhile (<= 999) primes
          a' <- [1,-1]
          b' <- [1{-,-1-}] -- if b is negative, then we get a negative number for n=0
          return ((a'*a,b'*b), count (a'*a) (b'*b))
        ((a,b),n) = maximumBy (comparing snd) candidates
    printf "n² + %dn + %d: %d primes\n%d * %d = %d\n" a b n a b (a*b)
  where count a b = length $ takeWhile isPrime quadratics
          where quadratics = [n^2 + a*n + b | n <- [0..]]
