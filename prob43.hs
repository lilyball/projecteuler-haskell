-- The number, 1406357289, is a 0 to 9 pandigital number because it is made up
-- of each of the digits 0 to 9 in some order, but it also has a rather
-- interesting sub-string divisibility property.
--
-- Let d_1 be the 1st digit, d_2 be the 2nd digit, and so on. In this way, we
-- note the following:
--
-- d_2d_3d_4=406  is divisible by 2
-- d_3d_4d_5=063  is divisible by 3
-- d_4d_5d_6=635  is divisible by 5
-- d_5d_6d_7=357  is divisible by 7
-- d_6d_7d_8=572  is divisible by 11
-- d_7d_8d_9=728  is divisible by 13
-- d_8d_9d_10=289 is divisible by 17
-- Find the sum of all 0 to 9 pandigital numbers with this property.
--

import Data.List (permutations, tails)
import Data.Numbers.Primes

-- all 0-9 pandigital numbers expressed as lists of digits
pandigitals :: [[Int]]
pandigitals = filter ((/= 0) . head) $ permutations [0..9]

listToInt :: [Int] -> Int
listToInt = foldl1 ((+).(10*))

groupBy3 :: [a] -> [[a]]
groupBy3 = takeWhile ((==3).length) . map (take 3) . tails

isDivisible :: Int -> Int -> Bool
isDivisible = ((== 0) .) . rem

-- takes a pandigital list
isInteresting :: [Int] -> Bool
isInteresting = and . flip (zipWith isDivisible) primes . nums
  where nums = map listToInt . tail . groupBy3

main = do
  let interestingPandigitals = filter isInteresting pandigitals
  print $ sum $ map listToInt interestingPandigitals
