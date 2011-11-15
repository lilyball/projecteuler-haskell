-- Starting with the number 1 and moving to the right in a clockwise direction a
-- 5 by 5 spiral is formed as follows:
--
--                                21 22 23 24 25
--                                20  7  8  9 10
--                                19  6  1  2 11
--                                18  5  4  3 12
--                                17 16 15 14 13
--
-- It can be verified that the sum of the numbers on the diagonals is 101.
--
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
-- formed in the same way?
--

import Data.List (scanl)

diagonals :: [Int]
diagonals = scanl (+) 1 deltas
  where deltas = [2,4..] >>= replicate 4

-- `diagonalsOfSpiral n` returns all the diagonals that belong to an
-- n x n spiral.
-- We know that there's 4 diagonals per spiral dimension n > 1, and
-- a single diagonal for n == 1, so we can just figure out how many
-- diagonals we expect and take that many.
-- Also note that n must be odd.
diagonalsOfSpiral :: Int -> [Int]
diagonalsOfSpiral n = take num diagonals
  where num = 1 + 4 * (n `div` 2)

main = do
  print $ sum $ diagonalsOfSpiral 1001
