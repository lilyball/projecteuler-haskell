-- It can be seen that the number, 125874, and its double, 251748, contain
-- exactly the same digits, but in a different order.
--
-- Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
-- contain the same digits.

import Control.Arrow
import Data.List ((\\), foldr1, sort)
import Euler.Numbers (numToDigits)

hasSameDigits :: [Int] -> Bool
hasSameDigits = allEq . map (sort . numToDigits)
  where allEq (x:xs) = all (x==) xs

main :: IO ()
main = do
  let ns = [map (i*) [1..6] | i <- [1..]]
  print $ head $ filter hasSameDigits $ ns
