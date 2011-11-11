-- A palindromic number reads the same both ways. The largest palindrome made from
-- the product of two 2-digit numbers is 9009 = 91x99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.
--

main = do
    print $ maximum $ filter (isPalindrome . show) $ products
  where products = [a * b | a <- [100..999], b <- [a..999]]
        isPalindrome x = x == reverse x
