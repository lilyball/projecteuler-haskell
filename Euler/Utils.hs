module Euler.Utils (
  isPalindrome
  ) where

import Control.Monad ((>>=))
import Control.Monad.Instances

isPalindrome :: String -> Bool
isPalindrome = reverse >>= (==)
