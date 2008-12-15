{-
http://projecteuler.net/index.php?section=problems&id=80

It is well known that if the square root of a natural number is
not an integer, then it is irrational. The decimal expansion of
such square roots is infinite without any repeating pattern at all.

The square root of two is 1.41421356237309504880..., and the digital
sum of the first one hundred decimal digits is 475.

For the first one hundred natural numbers, find the total of the
digital sums of the first one hundred decimal digits for all the
irrational square roots.
-}

import Data.Number.CReal
import Data.Char (digitToInt, isDigit)

isIntegral :: (RealFrac a) => a -> Bool
isIntegral x = fromIntegral (truncate x) == x
-- in this context, not integral == irrational
isIrrational :: (RealFrac a) => a -> Bool
isIrrational = not . isIntegral

root :: CReal -> CReal
root x = sqrt x

digits n x = take n $ int ++ frac
  where int = fst split
        frac = tail $ snd split
        split = span isDigit $ showCReal (n + 2) x

sum_of_root = sum . map digitToInt . digits 100 . root

main = do
    print $ sum roots
  where roots = [sum_of_root x | x <- [1..100], isIrrational (sqrt x)]
