-- In England the currency is made up of pound, £, and pence, p, and there are
-- eight coins in general circulation:
--
-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
-- It is possible to make £2 in the following way:
--
-- 1x£1 + 1x50p + 2x20p + 1x5p + 1x2p + 3x1p
-- How many different ways can £2 be made using any number of coins?
--

denominations :: [Int]
denominations = [1, 2, 5, 10, 20, 50, 100, 200]

-- combsForSum total denoms
-- pairs are (count,denom)
combsForSum :: Int -> [Int] -> [[(Int,Int)]]
combsForSum 0     ds = [[]]
combsForSum total [] = []
combsForSum total (d:ds) = do
    let dmax = total `quot` d
    dc <- [0..dmax]
    let total' = total - (dc*d)
    map ((dc,d):) $ combsForSum total' ds

main = do
  print $ length $ combsForSum 200 denominations
