-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
--
-- Find the sum of all numbers which are equal to the sum of the factorial of
-- their digits.
--
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.
--

import Data.Char (digitToInt)
import Control.Monad
import Control.Monad.Instances
import Data.Array

fac n
  | n < 1 = 1
  | n > 9 = fac' n
  | otherwise = a ! n
  where a = listArray (1,9) $ map fac' [1..9]
        fac' = product . enumFromTo 1

digits :: Int -> [Int]
digits = map digitToInt . show

predicate :: Int -> Bool
predicate = ap (==) (sum . map fac . digits)

-- 2540160 is an upper bound, because this is 7*9!, and therefore any larger
-- numbers cannot possibly be calculated from the factorials of its digits.
-- In practice, the actual upper bound is far smaller, but I don't know how to
-- calculate it.
main = do
  print $ sum $ filter predicate [10..2540160]
