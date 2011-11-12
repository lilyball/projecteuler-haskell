-- A permutation is an ordered arrangement of objects. For example, 3124 is one
-- possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
-- are listed numerically or alphabetically, we call it lexicographic order. The
-- lexicographic permutations of 0, 1 and 2 are:
--
-- 012   021   102   120   201   210
--
-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4,
-- 5, 6, 7, 8 and 9?
--

-- we can't use Data.List.permutations because it's not lexicographic,
-- and sorting would be too expensive.

import Data.List.Zipper
import Data.Char

perms :: [Int] -> [[Int]]
perms [] = [[]]
perms xs = perms' . fromList $ xs
  where perms' :: Zipper Int -> [[Int]]
        perms' z
          | endp z = []
          | otherwise = map (zc:) (perms zl) ++ perms' (right z)
          where zc = cursor z
                zl = toList $ delete z

main = do
  putStrLn $ map intToDigit $ perms [0..9] !! 999999
