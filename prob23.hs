{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
-- A perfect number is a number for which the sum of its proper divisors is
-- exactly equal to the number. For example, the sum of the proper divisors of
-- 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
--
-- A number n is called deficient if the sum of its proper divisors is less than
-- n and it is called abundant if this sum exceeds n.
--
-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
-- number that can be written as the sum of two abundant numbers is 24. By
-- mathematical analysis, it can be shown that all integers greater than 28123
-- can be written as the sum of two abundant numbers. However, this upper limit
-- cannot be reduced any further by analysis even though it is known that the
-- greatest number that cannot be expressed as the sum of two abundant numbers
-- is less than this limit.
--
-- Find the sum of all the positive integers which cannot be written as the sum
-- of two abundant numbers.
--

import Euler.Divisors
import Control.Monad (ap)
import Control.Monad.Instances()
import Data.List (tails)
import Data.Array.ST
import Control.Monad.ST

maxLimit :: (Integral n) => n
maxLimit = 28123 -- all integers higher are the sum of two abundant numbers

isAbundant :: (Integral a) => a -> Bool
isAbundant = ap (<) (sum.properDivisors)

-- Like Data.List.(\\) but assumes both lists are sorted
infix 5 \\
(\\) :: (Ord a) => [a] -> [a] -> [a]
[]        \\ _         = []
as        \\ []        = as
aa@(a:as) \\ bb@(b:bs) = case a `compare` b of
                    LT -> a : (as \\ bb)
                    EQ -> as \\ bs
                    GT -> aa \\ bs

abundantNums :: (Integral a) => [a]
abundantNums = filter isAbundant [1..]

-- Since all numbers > maxLimit are a sum
-- we can just count all abundant numbers up to maxLimit-1
sumOfTwoAbundant :: [Int]
sumOfTwoAbundant = sumToLimit ++ enumFrom (succ maxLimit)
  where sumToLimit = map fst $ filter snd $ runST $ getAssocs =<< ary
        ary :: ST s (STUArray s Int Bool)
        ary = do
            mary <- newArray (1,maxLimit) False
            let abundantss = init.tails $ takeWhile (< maxLimit) abundantNums
            mapM_ (process mary) abundantss
            return mary
        process mary ~aa@(a:_) = mapM_ step $ takeWhile ((<= maxLimit) . (a +)) aa
          where step b = writeArray mary (a+b) True

main :: IO ()
main = do
  print $ sum $ ([1..maxLimit] :: [Int]) \\ sumOfTwoAbundant
