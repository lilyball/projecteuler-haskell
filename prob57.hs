-- It is possible to show that the square root of two can be expressed as an
-- infinite continued fraction.
--
--  √2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
--
--  By expanding this for the first four iterations, we get:
--
--  1 + 1/2 = 3/2 = 1.5
--  1 + 1/(2 + 1/2) = 7/5 = 1.4
--  1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
--  1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
--
--  The next three expansions are 99/70, 239/169, and 577/408, but the eighth
--  expansion, 1393/985, is the first example where the number of digits in the
--  numerator exceeds the number of digits in the denominator.
--
--  In the first one-thousand expansions, how many fractions contain a numerator
--  with more digits than denominator?
--

-- According to the OEIS, this matches the sequence for a list of pairs
--   (1,1), (3,2), (7,5), (17,12), (41,29), ...
-- The pattern is (x,y), (x+2y,x+y)

pairs :: [(Integer,Integer)]
pairs = tail $ iterate step (1,1)
  where step :: (Integer,Integer) -> (Integer,Integer)
        step (x,y) = (x+2*y,x+y)

intLength :: Integer -> Int
intLength = length . show

main :: IO ()
main = do
  let pred = \(n,d) -> intLength n > intLength d
  print $ length $ filter pred $ take 1000 pairs
