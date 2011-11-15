-- A unit fraction contains 1 in the numerator. The decimal representation of
-- the unit fractions with denominators 2 to 10 are given:
--
-- 1/2  =   0.5
-- 1/3  =   0.(3)
-- 1/4  =   0.25
-- 1/5  =   0.2
-- 1/6  =   0.1(6)
-- 1/7  =   0.(142857)
-- 1/8  =   0.125
-- 1/9  =   0.(1)
-- 1/10 =   0.1
-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
-- seen that 1/7 has a 6-digit recurring cycle.
--
-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle
-- in its decimal fraction part.
--

import Data.List (maximumBy)
import Data.Ord (comparing)

-- `remainders n d` returns an infinite list of all the remainders after each
-- step of long division. This can be turned into digits by using something like
-- `(n*10 `div` d) : map (flip div d) (remainders n d)`.
-- This assumes that n < d. If it's not, then the first result will be a bit strange.
remainders :: Int -> Int -> [Int]
remainders n d = n' : remainders n' d
  where n' = (n*10) `mod` d

-- `cycleLength n d` returns the length of the cyclic part of n/d.
-- If n/d has a finite fractional part, this returns 0.
--
-- We can assume that n/d has _at most_ (d-2) possible digits before the cyclic part.
-- We know this because n < d, and if n ever repeats then we have a cycle.
-- Although since d might be 1, lets just say (d-1) instead.
-- We also know that once we're at the cyclic part, we can pick any remainder we want
-- and just wait for it to repeat, and that will be the cycle length. It doesn't matter
-- which one we pick because the cycle length will always be the same.
--
-- If we ever hit 0, then there's no cycle.
cycleLength :: Int -> Int -> Int
cycleLength n d = cycleLength' $ drop (d-1) $ takeWhile (/= 0) $ remainders n d
  where cycleLength' :: [Int] -> Int
        cycleLength' [] = 0
        cycleLength' (n:ns) = succ $ length $ takeWhile (/= n) ns

main :: IO ()
main = do
  print $ maximumBy (comparing snd) $ zip [1..] [cycleLength 1 d | d <- [1..999]]
