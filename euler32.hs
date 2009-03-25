{-
http://projecteuler.net/index.php?section=problems&id=32

We shall say that an n-digit number is pandigital if it makes use of all the digits 1
to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 x 186 = 7254, containing multiplicand,
multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be
written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only include it
once in your sum.
-}

import Utilities
import Data.List (sort)
import Data.Char (isDigit)
import Control.Arrow ((&&&))

isPandigitalStr :: String -> Bool
isPandigitalStr = (['1'..'9'] ==) . sort

-- | @isPandigital n@ returns whether there exists a combination of
--   multiplicand, multiplier, and product (@n@) that is pandigital
isPandigital :: Integer -> Bool
isPandigital = any (isPandigitalStr . filter isDigit . show) . combs
  where combs n = map (id &&& const n) $ divisorPairs n

main = do
    let inputs = [x | n <- [1..4], x <- permsN n ['1'..'9']]
    print $ (sum . filter isPandigital . map read) inputs
