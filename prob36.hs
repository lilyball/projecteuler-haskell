-- The decimal number, 585 = 1001001001_2 (binary), is palindromic in both bases.
--
-- Find the sum of all numbers, less than one million, which are palindromic in
-- base 10 and base 2.
--
-- (Please note that the palindromic number, in either base, may not include
-- leading zeros.)
--

import Numeric (showInt, showIntAtBase)
import Data.Char (intToDigit)
import Control.Monad.Instances

isPalindrome :: String -> Bool
isPalindrome = reverse >>= (==)

toBinary :: Integral a => a -> String
toBinary = flip (showIntAtBase 2 intToDigit) ""

toDecimal :: Integral a => a -> String
toDecimal = flip showInt ""

main :: IO ()
main = do
  print $ sum [i | i <- [1..999999], isPalindrome (toDecimal i), isPalindrome (toBinary i)]
