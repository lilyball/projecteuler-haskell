-- It was proposed by Christian Goldbach that every odd composite number can be
-- written as the sum of a prime and twice a square.
--
-- 9 = 7 + 2x1^2
-- 15 = 7 + 2x2^2
-- 21 = 3 + 2x3^2
-- 25 = 7 + 2x3^2
-- 27 = 19 + 2x2^2
-- 33 = 31 + 2x1^2
--
-- It turns out that the conjecture was false.
--
-- What is the smallest odd composite that cannot be written as the sum of a
-- prime and twice a square?
--

import Data.Numbers.Primes
import Euler.Numbers (isIntegral)

isTwiceSquare :: Int -> Bool
isTwiceSquare n = even n && (isIntegral $ sqrt $ fromIntegral (n `quot` 2))

composites :: [Int]
composites = filter (not.isPrime) [4..]

conjecture :: Int -> Bool
conjecture n = any test $ takeWhile (<n) primes
  where test p = isTwiceSquare (n - p)

main :: IO ()
main = do
  print $ head $ filter (not.conjecture) $ filter odd composites
