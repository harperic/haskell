-- What is the largest prime factor of the number 600851475143 ?
goal = 600851475143

-- First we need all primes
-- constructs all primes starting with 2...
-- does this take and add only those with an empty end?
-- that makes sense...because a prime would only have itself as a factor
-- in this case 1 does not count/is trivial. Thus, a prime only has itself
-- as a factor as determined by primeFactors
-- and lazy evaluation means that the list is only added to during execution
primes = 2 : filter (null . tail . primeFactors) [3,5..]
-- now to get the factors
-- how does this work?
-- we have a function that returns a value returned by another function
-- factor takes n and primes as inputs
-- and primes is already split into a head and tail...
primeFactors n = factor n primes
  where
    factor n (p:ps)
        -- n must be a prime b/c all pairs of integers have now been tried
        | p*p > n = [n]
        -- p is a factor of n. Add that factor to the list, reduce and continue
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        -- p is not a factor, so recurse over remaining primes
        | otherwise = factor n ps

-- now for haskell magic
problem_3 = last (primeFactors goal)
