-- An irrational decimal fraction is created by concatenating the positive
-- integers:
--
-- 0.123456789101112131415161718192021...
--
-- It can be seen that the 12th digit of the fractional part is 1.
--
-- If d_n represents the nth digit of the fractional part, find the value of the
-- following expression.
--
-- d_1 x d_10 x d_100 x d_1000 x d_10000 x d_100000 x d_1000000
--

import Data.Char (digitToInt)

digits :: [Int]
digits = concat [map digitToInt (show d) | d <- [0..]]

multiIndex :: [Int] -> [a] -> [a]
multiIndex is as = go 0 is as
  where go _ [] _     = []
        go _ (_:_) [] = error "multiIndex: index too large"
        go n ii@(i:is) aa@(a:as)
          | n == i    = a : go n is aa
          | n < i     = go (succ n) ii as
          | otherwise = error "multiIndex: indices not sorted"

main = do
  print $ product $ multiIndex [1,10,100,1000,10000,100000,1000000] digits
