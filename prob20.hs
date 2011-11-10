-- n! means n x (n x 1) x ... x 3 x 2 x 1
--
-- For example, 10! = 10 x 9 x ... x 3 x 2 x 1 = 3628800,
-- and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
--
-- Find the sum of the digits in the number 100!
--

import Data.Char (digitToInt)

factorial :: (Num n, Enum n) => n -> n
factorial = foldl (*) 1 . enumFromTo 1

main = do
  print $ sum $ map digitToInt $ show $ factorial 100
