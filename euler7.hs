{-
http://projecteuler.net/index.php?section=problems&id=7

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
we can see that the 6th prime is 13.

What is the 10001st prime number?
-}

import Sieve

main = do
  print $ primes !! 10000 -- 0 is the 1st prime