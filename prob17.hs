-- If the numbers 1 to 5 are written out in words: one, two, three, four, five,
-- then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out
-- in words, how many letters would be used?
--
--
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
-- forty-two) contains 23 letters and 115 (one hundred and fifteen) contains
-- 20 letters. The use of "and" when writing out numbers is in compliance with
-- British usage.
--

import Data.Char (isLetter)
import Data.List (intercalate)

wordify :: Int -> String
wordify 0 = "zero"
wordify n = intercalate " " $ wordify' n False
  where wordify' :: Int -> Bool -> [String]
        wordify' 0 _ = []
        wordify' n and
          | n < 0     = ["negative"] ++ wordify' (abs n) False
          | n >= 1000 = wordify' (n `div` 1000) False ++ ["thousand"] ++ wordify' (n `mod` 1000) True
          | n >= 100  = wordify' (n `div` 100)  False ++ ["hundred"] ++ wordify' (n `mod` 100) True
          | n >= 10   = (if and then ["and"] else []) ++ [wordifyTens n]
          | otherwise = (if and then ["and"] else []) ++ [wordifyOnes n]
        wordifyTens :: Int -> String
        wordifyTens 10 = "ten"
        wordifyTens 11 = "eleven"
        wordifyTens 12 = "twelve"
        wordifyTens 13 = "thirteen"
        wordifyTens 14 = "fourteen"
        wordifyTens 15 = "fifteen"
        wordifyTens 16 = "sixteen"
        wordifyTens 17 = "seventeen"
        wordifyTens 18 = "eighteen"
        wordifyTens 19 = "nineteen"
        wordifyTens 20 = "twenty"
        wordifyTens 30 = "thirty"
        wordifyTens 40 = "forty"
        wordifyTens 50 = "fifty"
        wordifyTens 60 = "sixty"
        wordifyTens 70 = "seventy"
        wordifyTens 80 = "eighty"
        wordifyTens 90 = "ninety"
        wordifyTens n = wordifyTens (t * 10) ++ '-' : wordifyOnes o
          where (t,o) = n `divMod` 10
        wordifyOnes 1 = "one"
        wordifyOnes 2 = "two"
        wordifyOnes 3 = "three"
        wordifyOnes 4 = "four"
        wordifyOnes 5 = "five"
        wordifyOnes 6 = "six"
        wordifyOnes 7 = "seven"
        wordifyOnes 8 = "eight"
        wordifyOnes 9 = "nine"

countLetters :: String -> Int
countLetters = foldr (\c s -> if isLetter c then succ s else s) 0

main = do
  print $ sum $ map countLetters [wordify n | n <- [1..1000]]
