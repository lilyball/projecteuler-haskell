-- Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
-- containing over five-thousand first names, begin by sorting it into
-- alphabetical order. Then working out the alphabetical value for each name,
-- multiply this value by its alphabetical position in the list to obtain a name
-- score.
--
-- For example, when the list is sorted into alphabetical order, COLIN, which is
-- worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
-- would obtain a score of 938  53 = 49714.
--
-- What is the total of all the name scores in the file?
--

import Data.Char (isAscii, isUpper, ord)
import Data.List (mapAccumL, sort)

letterScore :: Char -> Int
letterScore c
  | isAscii c && isUpper c = ord c - ord 'A' + 1
  | otherwise = 0

wordScore :: String -> Int
wordScore = sum . map letterScore

nameScores :: [String] -> [Int]
nameScores = snd . mapAccumL f 1
  where f :: Int -> String -> (Int, Int)
        f n name = (succ n, n * wordScore name)

main = do
  contents <- readFile "prob22.txt"
  let names = read $ '[' : contents ++ "]"
  print $ sum $ nameScores $ sort names
