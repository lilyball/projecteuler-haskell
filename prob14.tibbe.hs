-- Direct translation of C program by tibbe

{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Data.Bits

main :: IO ()
main = putStrLn $ "longest: " ++ show longest ++ " (" ++ show terms ++ ")"
  where IP longest terms = euler14

data IntPair = IP {-# UNPACK #-} !Int {-# UNPACK #-} !Int

euler14 :: IntPair
euler14 = go 1 0 0
  where
    go :: Int -> Int -> Int -> IntPair
    go i !longest !terms
        | i <= 1000000 = while i 1 longest terms
        | otherwise    = IP longest terms
      where
        while :: Int -> Int -> Int -> Int -> IntPair
        while 1 !_ !longest !terms = go (i+1) longest terms
        while j thisTerms longest terms =
            let thisTerms' = thisTerms + 1
                IP terms' longest' = if thisTerms > terms
                                     then IP thisTerms i
                                     else IP terms longest
                j' = if j .&. 1 == 0
                     then j `quot` 2
                     else 3 * j + 1
            in while j' thisTerms' longest' terms'

