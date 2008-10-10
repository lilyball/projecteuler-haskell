{-
http://projecteuler.net/index.php?section=problems&id=9

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

        a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}

triple :: Int -> Int -> Maybe Int
triple a b = if frac == 0.0 then Just int else Nothing
  where c = sqrt $ fromIntegral $ a^2 + b^2
        (int, frac) = properFraction c

main = do
  print $ head $ [a*b*c | a <- [1..1000], b <- [a..1000],
                          Just c <- [triple a b], a+b+c == 1000]
