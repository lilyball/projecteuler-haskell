{-
http://projecteuler.net/index.php?section=problems&id=31

In England the currency is made up of pound, £, and pence, p,
and there are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

    1x£1 + 1x50p + 2x20p + 1x5p + 1x2p + 3x1p

How many different ways can £2 be made using any number of coins?
-}

import Data.Array

change :: Int -> Int
change n = memo ! (n,8)
  where memo :: Array (Int,Int) Int
        memo = array bounds [(i, uncurry f i) | i <- range bounds]
        bounds = ((0,1),(n,8))
        f :: Int -> Int -> Int
        f 0 _ = 1
        f i 1 = 1
        f i p = a + b
          where a | i-p' < 0 = 0
                  | otherwise = memo ! ((i-p'), p)
                b = memo ! (i, (p-1))
                p' = pence ! p
        pence :: Array Int Int
        pence = listArray (1,8) [1, 2, 5, 10, 20, 50, 100, 200]

main = do
  print $ change 200
