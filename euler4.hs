{-
http://projecteuler.net/index.php?section=problems&id=4

A palindromic number reads the same both ways. The largest palindrome
made from the product of two 2-digit numbers is 9009 = 91  99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}

import Data.List

main = do
    print $ head $ sortBy revCompare palindromes
  where palindromes = [a*b | a <- range, b <- range, isPalindrome (a*b)]
        range = [100..999]

isPalindrome x = let sx = show x in sx == reverse sx

revCompare a b = case compare a b of
                 LT -> GT
                 EQ -> EQ
                 GT -> LT
