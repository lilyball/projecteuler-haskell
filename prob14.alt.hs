-- An attempt at a more imperative implementation of prob14.
-- Performs worse.

import Control.Monad.State.Lazy
import Control.Monad (when)

loop :: Int -> Int
loop upperBound = fst $ execState (loop' 1 upperBound) (0,0)
  where loop' :: Int -> Int -> State (Int,Int) ()
        loop' n upperBound
          | n > upperBound = return ()
          | otherwise = do
              (i,m) <- get
              let c = collatz n
              when (c > m) $ put (n,c)
              loop' (succ n) upperBound

collatz 1 = 1
collatz n = succ $ collatz $ step n
  where step n
          | even n    = n `div` 2
          | otherwise = 3*n + 1

main = do
  print $ loop 999999
