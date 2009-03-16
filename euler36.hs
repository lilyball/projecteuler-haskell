{-
  The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.

  Find the sum of all numbers, less than one million, which are palindromic
  in base 10 and base 2.

  (Please note that the palindromic number, in either base, may not include leading zeros.)
-}

import Data.Maybe
import Control.Monad
import Data.List

isPalindrome :: Int -> Bool
isPalindrome i = i' == reverse i'
  where i' = show i

binPalindromeSeq :: [Int]
binPalindromeSeq = 1 : sort [readBin b | a <- source, b <- mirrors a]
  where source :: [[Int]]
        source = [(1:a) | n <- [0..9], a <- replicateM n [0,1]]
        mirrors :: [Int] -> [[Int]]
        mirrors x = [mirror Nothing x, mirror (Just 0) x, mirror (Just 1) x]
          where mirror :: Maybe Int -> [Int] -> [Int]
                mirror center x = x ++ (maybe [] (:[]) center) ++ reverse x
        readBin :: [Int] -> Int
        readBin [] = 0
        readBin xs = foldl1 ((+) . (2*)) xs

main = do
    print $ (sum . filter isPalindrome) (takeWhile (<1000000) $ binPalindromeSeq)
