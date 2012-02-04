-- There are exactly ten ways of selecting three from five, 12345:
--
--            123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
--
-- In combinatorics, we use the notation, ^5C_3 = 10.
--
-- In general,
--
-- ^nC_r = n!/(r!(nr)!), where r ≤ n, n! = nx(n-1)x...x3x2x1, and 0! = 1.
--
-- It is not until n = 23, that a value exceeds one-million: ^23C_10 = 1144066.
--
-- How many, not necessarily distinct, values of  ^nC_r, for 1 ≤ n ≤ 100, are
-- greater than one-million?
--

import Control.Monad
import Control.Monad.Instances
import Math.Combinatorics.Exact.Binomial (choose)

main = do
  print $ length $ filter (>1000000) $ [n `choose` r | n <- [1..100], r <- [1..n]]
