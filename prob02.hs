fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main = do
  print $ sum $ filter even $ takeWhile (< 4000000) fibs
