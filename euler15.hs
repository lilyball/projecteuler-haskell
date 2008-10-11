{-
http://projecteuler.net/index.php?section=problems&id=15

Starting in the top left corner of a 2x2 grid, there are 6 routes
(without backtracking) to the bottom right corner.

    (note: see URL for diagram)

How many routes are there through a 20x20 grid?
-}

import Data.Array

routes width height = grid ! (0, 0)
  where grid = array bounds [(i, f i) | i <- range bounds]
        bounds = ((0, 0), (width, height))
        f (w, h)
            | w == width && h == height = 1
            | w == width = grid ! (w, h+1)
            | h == height = grid ! (w+1, h)
            | otherwise = (grid ! ((w+1), h)) + (grid ! (w, (h+1)))

main = do
  print $ routes 20 20
