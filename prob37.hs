-- The number 3797 has an interesting property. Being prime itself, it is
-- possible to continuously remove digits from left to right, and remain prime
-- at each stage: 3797, 797, 97, and 7. Similarly we can work from right to
-- left: 3797, 379, 37, and 3.
--
-- Find the sum of the only eleven primes that are both truncatable from left to
-- right and right to left.
--
-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
--

import Data.Numbers.Primes
import Data.List

primeDigits = takeWhile (<10) primes

build :: (String -> String -> String) -> [Int]
build plus = concat $ unfoldr step primeDigits
  where step xs = toMaybe $ filter isPrime $ sort [combine d x | x <- xs, d <- [1..9]]
          where combine :: Int -> Int -> Int
                combine d x = read $ show d `plus` show x
        toMaybe [] = Nothing
        toMaybe xs = Just (xs, xs)

buildLeft = build (++)
buildRight = build (flip (++))

-- Version of intersect that assumes sorted lists
intersect' :: (Ord a) => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' _ [] = []
intersect' ass@(a:as) bss@(b:bs)
  | a == b    = a : intersect as bs
  | a < b     = intersect as bss
  | otherwise = intersect ass bs

main = do
  print $ sum $ take 11 $ intersect' buildLeft buildRight
