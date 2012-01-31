module Euler.Numbers (
  isIntegral,
  numToDigits, digitsToNum,
  triangles, isTriangle,
  pentagonals, isPentagonal,
  hexagonals, isHexagonal
  ) where

import Data.List (unfoldr)

-- Utility functions

isIntegral :: (RealFrac n) => n -> Bool
isIntegral = (==0) . snd . properFraction

numToDigits :: (Integral n) => n -> [Int]
numToDigits 0 = [0]
numToDigits n
  | n < 0 = error "numToDigits: negative input"
  | otherwise = reverse $ unfoldr step n
  where step 0 = Nothing
        step n = let (n',d) = n `quotRem` 10
                 in Just (fromIntegral d, n')

digitsToNum :: (Integral n) => [Int] -> n
digitsToNum = foldl ((.fromIntegral).(+).(10*)) 0

-- Triangle numbers

triangles :: (Integral n) => [n]
triangles = map go [1..]
  where go n = n * (n+1) `quot` 2

isTriangle :: (Integral n) => n -> Bool
isTriangle t = isIntegral n
  where n = (-1 + sqrt (1 + 8*fromIntegral t)) / 2

-- Pentagonal numbers

pentagonals :: (Integral n) => [n]
pentagonals = map go [1..]
  where go n = n * (3*n - 1) `quot` 2

isPentagonal :: (Integral n) => n -> Bool
isPentagonal p = isIntegral n
  where n = ((1/2) + sqrt (1/4 + 6*fromIntegral p)) / 3

-- Hexagonal numbers

hexagonals :: (Integral n) => [n]
hexagonals = map go [1..]
  where go n = n * (2*n-1)

isHexagonal :: (Integral n) => n -> Bool
isHexagonal h = isIntegral n
  where n = (1 + sqrt (1 + 8*fromIntegral h)) / 4
