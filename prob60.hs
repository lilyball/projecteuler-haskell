-- The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes
-- and concatenating them in any order the result will always be prime. For
-- example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these
-- four primes, 792, represents the lowest sum for a set of four primes with
-- this property.
--
-- Find the lowest sum for a set of five primes for which any two primes
-- concatenate to produce another prime.
--

import qualified Data.Numbers.Primes as P
import Data.List

-- Test the new number pairwise with all existing numbers
primeTest :: [Int] -> Int -> Bool
primeTest ns new = all (test new) ns
  where test :: Int -> Int -> Bool
        test a b = test' a b && test' b a
        test' a b = P.isPrime $ read $ show a ++ show b

primes :: [Int]
primes = take 1200 P.primes

-- return all solutions of the given length
solutions :: Int -> [[Int]]
solutions l = map reverse $ iterate go (map (:[]) primes) !! (pred l)
  where go :: [[Int]] -> [[Int]]
        go nss = concatMap generate nss
        generate :: [Int] -> [[Int]]
        generate ns = map (:ns) $ filter (primeTest ns) $ primes'
          where primes' = dropWhile (<= head ns) primes

main :: IO ()
main = do
  print $ sum $ head $ solutions 5
