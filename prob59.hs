-- Each character on a computer is assigned a unique code and the preferred
-- standard is ASCII (American Standard Code for Information Interchange). For
-- example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
--
-- A modern encryption method is to take a text file, convert the bytes to
-- ASCII, then XOR each byte with a given value, taken from a secret key. The
-- advantage with the XOR function is that using the same encryption key on the
-- cipher text, restores the plain text; for example, 65 XOR 42 = 107, then 107
-- XOR 42 = 65.
--
-- For unbreakable encryption, the key is the same length as the plain text
-- message, and the key is made up of random bytes. The user would keep the
-- encrypted message and the encryption key in different locations, and without
-- both "halves", it is impossible to decrypt the message.
--
-- Unfortunately, this method is impractical for most users, so the modified
-- method is to use a password as a key. If the password is shorter than the
-- message, which is likely, the key is repeated cyclically throughout the
-- message. The balance for this method is using a sufficiently long password
-- key for security, but short enough to be memorable.
--
-- Your task has been made easy, as the encryption key consists of three lower
-- case characters. Using prob59.txt, a file containing the encrypted ASCII
-- codes, and the knowledge that the plain text must contain common English
-- words, decrypt the message and find the sum of the ASCII values in the
-- original text.
--

import Data.Bits (xor)
import Data.Char (isSpace,isAlphaNum)
import Data.Ord (comparing)
import Control.Monad (replicateM)

getCiphertext :: IO String
getCiphertext = do
  source <- fmap (takeWhile (not.isSpace)) $ readFile "prob59.txt"
  let bytes :: [Int]
      bytes = read $ "[" ++ source ++ "]"
  return $ map toEnum bytes

-- all possible passwords
passwords :: [String]
passwords = replicateM 3 ['a'..'z']

-- decrypt password ciphertext
decrypt :: String -> String -> String
decrypt ciphertext pass = toString decrypted
  where decrypted :: [Int]
        decrypted = zipWith xor (fromString ciphertext) (cycle $ fromString pass)
        toString :: [Int] -> String
        toString = map toEnum
        fromString :: String -> [Int]
        fromString = map fromEnum

isGood :: String -> Bool
isGood = and . map (flip any tests . flip ($))
  where tests = [isAlphaNum, isSpace, isGoodPunct]
        isGoodPunct = flip elem ",.()'\"?!@#$%&*:;"

main :: IO ()
main = do
  cipher <- getCiphertext
  let decryptions = map (decrypt cipher) passwords
      validDecryptions = filter isGood decryptions
  -- we've experimentally confirmed that validDecryptions is 1 entry
  let best = head validDecryptions
  print best
  print $ sum $ map fromEnum best
