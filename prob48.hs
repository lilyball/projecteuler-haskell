-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
--
-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
--

-- naïve solution first
-- Just do the actual math
-- Turns out it's incredibly fast

main = do
  let n = sum [i^i | i <- [1..1000]]
  let s = show n
  print $ drop (length s - 10) s
