-- If p is the perimeter of a right angle triangle with integral length sides,
-- {a,b,c}, there are exactly three solutions for p = 120.
--
-- {20,48,52}, {24,45,51}, {30,40,50}
--
-- For which value of p ≤ 1000, is the number of solutions maximised?
--

import Control.Arrow (second)
import Control.Monad
import Control.Monad.Instances
import Data.List (maximumBy)
import Data.Function (on)
import Data.Array

solutionsForPerimiter :: Int -> [(Int,Int,Int)]
solutionsForPerimiter p = do
    a <- [2..(p `quot` 3)]
    b <- [a..(p `quot` 2)]
    let c = p-a-b
        ab = a*a + b*b
    guard (perfectSquares ! c == ab)
    return (a,b,c)
  where perfectSquares :: Array Int Int
        perfectSquares = array (1,p) [(i, i*i) | i <- [1..p]]

main :: IO ()
main = do
  print $ second length $ maximumBy (compare `on` length.snd) $ map (ap (,) solutionsForPerimiter) [1..1000]
