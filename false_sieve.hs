import Data.List

primes = sieve [2..1000] 
  where sieve (p:xs) = 
          p : sieve [x | x <- xs, x `mod` p /= 0]

primesTo m = sieve [2..m]       {- (\\) is set-difference for unordered lists -}
             where 
             sieve (x:xs) = x : sieve (xs Data.List.\\ [x,x+x..m])
             sieve [] = []