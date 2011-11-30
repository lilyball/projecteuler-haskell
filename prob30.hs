-- Surprisingly there are only three numbers that can be written as the sum of
-- fourth powers of their digits:
--
-- 1634 = 1^4 + 6^4 + 3^4 + 4^4
-- 8208 = 8^4 + 2^4 + 0^4 + 8^4
-- 9474 = 9^4 + 4^4 + 7^4 + 4^4
-- As 1 = 1^4 is not a sum it is not included.
--
-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.
--
-- Find the sum of all the numbers that can be written as the sum of fifth
-- powers of their digits.
--

import Data.Char (digitToInt)

isDigitsPowerEqual :: Int -> Int -> Bool
isDigitsPowerEqual p i
    | i <= 9    = False
    | otherwise = i == sum (map (^p) (digits i))
  where digits :: Int -> [Int]
        digits = map digitToInt . show

maxSumForDigitCount :: Int -> Int -> Int
maxSumForDigitCount p i = (9^p) * i

numberOfDigits :: Int -> Int
numberOfDigits = ceiling . logBase 10 . fromIntegral

main = do
  let maxSum :: Int -> Int -> Int
      maxSum p i = (9^p) * i
      digitCounts = takeWhile (\i -> maxSum 5 i >= 10^(i-1)) [1..]
      nums = concatMap (\i -> enumFromTo (10^(i-1)) (10^i-1)) digitCounts
  print $ sum $ filter (isDigitsPowerEqual 5) nums
