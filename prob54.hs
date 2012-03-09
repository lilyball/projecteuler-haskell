-- In the card game poker, a hand consists of five cards and are ranked, from
-- lowest to highest, in the following way:
--
-- * High Card: Highest value card.
-- * One Pair: Two cards of the same value.
-- * Two Pairs: Two different pairs.
-- * Three of a Kind: Three cards of the same value.
-- * Straight: All cards are consecutive values.
-- * Flush: All cards of the same suit.
-- * Full House: Three of a kind and a pair.
-- * Four of a Kind: Four cards of the same value.
-- * Straight Flush: All cards are consecutive values of same suit.
-- * Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
-- * The cards are valued in the order:
-- * 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
--
-- If two players have the same ranked hands then the rank made up of the
-- highest value wins; for example, a pair of eights beats a pair of fives (see
-- example 1 below). But if two ranks tie, for example, both players have a pair
-- of queens, then highest cards in each hand are compared (see example 4
-- below); if the highest cards tie then the next highest cards are compared,
-- and so on.
--
-- Consider the following five hands dealt to two players:
--
-- Hand     Player 1          Player 2         Winner
--  1    5H 5C 6S 7S KD     2C 3S 8S 8D TD    Player 2
--        Pair of Fives     Pair of Eights
--  2    5D 8C 9S JS AC     2C 5C 7D 8S QH    Player 1
--      Highest card Ace  Highest card Queen
--  3    2D 9C AS AH AC     3D 6D 7D TD QD    Player 2
--        Three Aces     Flush with Diamonds
--  4    4D 6S 9H QH QC     3D 6D 7H QD QS    Player 1
--       Pair of Queens     Pair of Queens
--     Highest card Nine  Highest card Seven
--  5    2H 2D 4C 4D 4S     3C 3D 3S 9S 9D    Player 1
--         Full House         Full House
--      With Three Fours   With Three Threes
--
--
-- The file, prob54.txt, contains one-thousand random hands dealt to two
-- players. Each line of the file contains ten cards (separated by a single
-- space): the first five are Player 1's cards and the last five are Player 2's
-- cards. You can assume that all hands are valid (no invalid characters or
-- repeated cards), each player's hand is in no specific order, and in each hand
-- there is a clear winner.
--
-- How many hands does Player 1 win?

import Control.Arrow ((***))
import Data.Function (on)
import Data.List (groupBy, maximumBy, nub, sort, sortBy)
import Data.Maybe
import Data.Ord (comparing)
import Text.ParserCombinators.ReadPrec
import Text.Read

data Suit = Clubs | Spades | Diamonds | Hearts deriving (Eq)
data Card = Card {cardValue :: Int, suitValue :: Suit} deriving (Eq)
type Hand = [Card]
data Rank = HighCard
          | Pair Card
          | TwoPair Card Card -- high pair first
          | ThreeOfAKind Card
          | Straight Card -- lowest value in the straight
          | Flush -- suit is irreleavnt for ranking purposes
          | FullHouse Card -- three of a kind value
          | FourOfAKind Card
          | StraightFlush Card -- lowest value in the straight
          | RoyalFlush
          deriving (Eq, Ord, Show)
data Player = Player1 | Player2 deriving (Show, Eq)

instance Ord Card where
  compare (Card v _) (Card v' _) = compare v v'

instance Show Card where
  show (Card n s)
      | n <  10 = shows n $ show' s
      | n == 10 = "T" ++ show' s
      | n == 11 = "J" ++ show' s
      | n == 12 = "Q" ++ show' s
      | n == 13 = "K" ++ show' s
      | n == 14 = "A" ++ show' s
    where show' Clubs = "C"
          show' Spades = "S"
          show' Diamonds = "D"
          show' Hearts = "H"

instance Read Card where
  readPrec = do
      v <- readValue
      s <- readSuit
      return $ Card v s
    where readValue :: ReadPrec Int
          readValue = readNumber +++ readFace
            where readNumber = readS_to_Prec readsPrec
                  readFace = do
                    c <- get
                    case c of
                      'T' -> return 10
                      'J' -> return 11
                      'Q' -> return 12
                      'K' -> return 13
                      'A' -> return 14
                      _   -> pfail
          readSuit :: ReadPrec Suit
          readSuit = do
            c <- get
            case c of
              'C' -> return Clubs
              'S' -> return Spades
              'D' -> return Diamonds
              'H' -> return Hearts
              _   -> pfail

parseHands :: String -> (Hand, Hand) -- the string is cards separated by spaces
parseHands = (map read *** map read) . splitAt 5 . words

allEq :: (Eq a) => [a] -> Bool
allEq (x:xs) = all (== x) xs
allEq [] = True

classify :: Hand -> Rank
classify cs
    | isFlush && isStraight && cardValue lowCard == 10 = RoyalFlush
    | isFlush && isStraight = StraightFlush lowCard
    | length (head sortedGroups) == 4 = FourOfAKind (head.head $ sortedGroups)
    | map length sortedGroups == [3,2] = FullHouse (head.head $ sortedGroups)
    | isFlush = Flush
    | isStraight = Straight lowCard
    | length (head sortedGroups) == 3 = ThreeOfAKind (head.head $ sortedGroups)
    | map length sortedGroups == [2,2,1] = TwoPair (head.head $ sortedGroups) (head.head.tail $ sortedGroups)
    | length (head sortedGroups) == 2 = Pair (head.head $ sortedGroups)
    | otherwise = HighCard
  where lowCard :: Card
        lowCard = head.head $ reverse sortedGroups
        sortedGroups :: [[Card]]
        sortedGroups = sortBy (flip compare `on` length) $ groupBy (((==EQ).).compare) $ sortBy (flip compare) cs
        isFlush :: Bool
        isFlush = allEq (map suitValue cs)
        isStraight :: Bool
        isStraight = and $ zipWith (==) (enumFrom (cardValue lowCard)) (map cardValue (sort cs))

scoreHands :: Hand -> Hand -> Maybe Player -- returns Nothing if tied
scoreHands h1 h2 = case compare (classify h1) (classify h2) of
                     GT -> Just Player1
                     LT -> Just Player2
                     EQ -> scoreHands' -- compare high cards
  where scoreHands' = case compare (reverse $ sort h1) (reverse $ sort h2) of
                        GT -> Just Player1
                        LT -> Just Player2
                        EQ -> Nothing

main :: IO ()
main = do
  text <- readFile "prob54.txt"
  let hands = map parseHands $ lines text
      scores = mapMaybe (uncurry scoreHands) hands
  print $ length $ filter (==Player1) scores
