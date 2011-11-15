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

import Data.List (find, tails, maximumBy)
import Data.Ord (comparing)

-- We need to implement long-hand division, because we want a never-ending
-- stream of fractional digits.
divide :: Int -> Int -> (Int,[Int])
divide n d = (int,divide' (rem*10) d)
  where (int,rem) = n `divMod` d
        divide' n d
          | d > n = 0 : divide' (n*10) d
          | otherwise = if dig > 9
                        then error "invalid digit"
                        else dig : divide' (rem*10) d
          where (dig,rem) = n `divMod` d

-- cycleLength just runs a heuristic.
-- If we match either 30 digits, or 5 repetitions of the pattern, whichever
-- comes second, then we declare it to be cyclic. A non-cyclic (e.g. terminating)
-- stream is considered to have a cycle length of 0.
-- Since infinite fractions in Real numbers are, by definition, cyclic, we
-- can be clever here in the way we handle checking both longer sequences and
-- sequences that start further into the stream. We can check a 1-digit pattern
-- at position 1, then a 2-digit pattern at position 2, then a 3-digit pattern
-- at position 3, etc. Once we find such a pattern, run the cycle test once more
-- in case we matched a longer pattern where a shorter one would have worked.
cycleLength :: [Int] -> Int
cycleLength [] = 0
cycleLength ns = maybe 0 shrink $ find (not.null) $ map findCycle $ zip (tails ns) [1..]
  where findCycle :: ([Int],Int) -> [Int]
        findCycle ([],_) = []
        findCycle (ns,l) = if length ns' < len then [] else findCycle'
          where len = max 30 (l*5)
                ns' = take len ns
                findCycle' :: [Int]
                findCycle' = if take len (cycle ns'') == ns' then ns'' else []
                  where ns'' = take l ns
        shrink :: [Int] -> Int
        shrink ns = length $ maybe ns id $ find shrink' [take n ns | n <- divs]
          where divs = filter ((== 0) . (mod l)) [1..ul]
                ul = l `div` 2
                l = length ns
                shrink' = (ns ==) . take l . cycle

main :: IO ()
main = do
  print $ maximumBy (comparing snd) [(i,cycleLength d) | i <- [1..999], let (_,d) = divide 1 i]
