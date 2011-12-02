-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1
-- through 5 pandigital.
--
-- The product 7254 is unusual, as the identity, 39 x 186 = 7254, containing
-- multiplicand, multiplier, and product is 1 through 9 pandigital.
--
-- Find the sum of all products whose multiplicand/multiplier/product identity
-- can be written as a 1 through 9 pandigital.
--
-- HINT: Some products can be obtained in more than one way so be sure to only
-- include it once in your sum.
--

import qualified Euler.Divisors as D
import Control.Monad (ap, liftM2)
import Control.Monad.Instances
import Data.List (group, sort)

divisors :: Int -> [(Int,Int)]
divisors = tail . takeWhile (uncurry (<=)) . ap zip reverse . D.divisors

-- `candidates` is a list of all numbers that may be valid products.
-- Since we're aiming for 1-9 pandigital, the number itself cannot have any
-- repeating digits. What's more, it can't be more than 5 digits, because
-- the total number of digits of the product and its divisors must be equal
-- to 9.(a 6-digit number would require divisors of lengths 1 and 2, and
-- 99*9 cannot posisbly be 6 digits).
-- Similarly it mus tbe at least 3 digits, because a 2-digit number with
-- its divisors cannot possibly be 9 digits.
candidates :: [Int]
candidates = filter valid [123..98765]
  where valid = all ((==1) . length) . group . sort . show

main = do
    print $ sum $ products
  where products = filter test candidates
        test = any valid . ap (map . flip (uncurry (,,))) divisors
        valid (a,b,c) = "123456789" == (sort $ concat $ map show [a,b,c])
