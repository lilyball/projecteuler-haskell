-- The fraction 49/98 is a curious fraction, as an inexperienced mathematician
-- in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which
-- is correct, is obtained by cancelling the 9s.
--
-- We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
--
-- There are exactly four non-trivial examples of this type of fraction, less
-- than one in value, and containing two digits in the numerator and
-- denominator.
--
-- If the product of these four fractions is given in its lowest common terms,
-- find the value of the denominator.
--

import Data.Ratio
import Control.Monad (guard)

data RawRatio = RawRatio !Integer !Integer deriving (Show)

toRatio :: RawRatio -> Rational
toRatio (RawRatio a b) = a % b

instance Eq RawRatio where
  a == b = toRatio a == toRatio b

fractions :: [RawRatio]
fractions = do
  nten <- [1..9]
  none <- [1..9]
  let n = nten*10+none
  let dten = none
  done <- [1..9]
  let d = dten*10+done
  guard $ n < d
  return $ RawRatio n d

isCancellable :: RawRatio -> Bool
isCancellable r@(RawRatio n d) = r == r'
  where ntens = n `quot` 10
        dones = d `rem` 10
        r' = RawRatio ntens dones

main = do
  print $ product $ map toRatio $ filter isCancellable fractions
