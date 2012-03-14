-- Starting with 1 and spiralling anticlockwise in the following way, a square
-- spiral with side length 7 is formed.
--
--                            37 36 35 34 33 32 31
--                            38 17 16 15 14 13 30
--                            39 18  5  4  3 12 29
--                            40 19  6  1  2 11 28
--                            41 20  7  8  9 10 27
--                            42 21 22 23 24 25 26
--                            43 44 45 46 47 48 49
--
-- It is interesting to note that the odd squares lie along the bottom right
-- diagonal, but what is more interesting is that 8 out of the 13 numbers lying
-- along both diagonals are prime; that is, a ratio of 8/13  62%.
--
-- If one complete new layer is wrapped around the spiral above, a square spiral
-- with side length 9 will be formed. If this process is continued, what is the
-- side length of the square spiral for which the ratio of primes along both
-- diagonals first falls below 10%?
--

import Data.List (find, genericLength)
import Data.Maybe (fromJust)
import Data.Numbers.Primes (isPrime)
import Data.Ratio

-- definition of `diagonals` taken from prob28.hs
diagonals :: (Integral i) => [i]
diagonals = scanl (+) 1 deltas
  where deltas = [2,4..] >>= replicate 4

-- `diagonalsOfSpiral` returns a list of lists, where each sublist
-- is the diagonals of the next layer in the spiral
diagonalsOfSpiral :: (Integral i) => [[i]]
diagonalsOfSpiral = d : map (take 4) (iterate (drop 4) ds)
  where (d,ds) = splitAt 1 diagonals

ratiosOfSpiral :: [Rational]
ratiosOfSpiral = zipWith (%) countOfPrimes counts
  where countOfPrimes :: [Integer]
        countOfPrimes = scanl1 (+) $ map (genericLength . filter isPrime) ds
        counts :: [Integer]
        counts = scanl1 (+) $ map genericLength ds
        ds = diagonalsOfSpiral

sideLengths :: [Int]
sideLengths = [1,3..]

main :: IO ()
main = do
  let ds = tail $ zip sideLengths ratiosOfSpiral
  print $ fst $ fromJust $ find ((<0.1) . fromRational . snd) ds
