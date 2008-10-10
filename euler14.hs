{-
http://projecteuler.net/index.php?section=problems&id=14

The following iterative sequence is defined for the set of positive integers:

        n -> n/2 (n is even)
        n -> 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

        13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1

It can be seen that this sequence (starting at 13 and finishing at 1)
contains 10 terms. Although it has not been proved yet (Collatz Problem),
it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
-}

import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Map as Map
import Control.Monad.State.Strict
import Control.Monad (liftM)

type ChainState = State (Map.Map Integer [Integer])

runChainState :: ChainState a -> a
runChainState cs = evalState cs $ Map.singleton 1 [1]

chain :: Integer -> ChainState [Integer]
chain n = do
    map <- get
    case Map.lookup n map of
      Nothing -> do
            ns <- chain' n
            put $ Map.insert n ns map
            return ns
      Just ns -> return ns
  where chain' n
            | even n    = liftM (n:) $ chain $ n `div` 2
            | otherwise = liftM (n:) $ chain $ 3*n + 1

main = do
  let chains = sequence $ map chain [1..999999]
  print $ head $ head $ sortBy (flip compare `on` length) $ runChainState chains
