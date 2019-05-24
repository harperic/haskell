-- By considering the terms in the Fibonacci sequence whose
-- values do not exceed four million, find the sum of the even-valued terms.

-- function to calculate the nth fib
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

-- lets create a list of the fibs
-- remember, haskell has no problems with infinite numbers!
fibs = [fib x | x <- [0,1..]]

-- and now for the solution
problem_2 = sum[ x | x <- takeWhile (<= 4000000) fibs, even x]

-- another possible solution, much faster
problem_2' = sum [x | x <- takeWhile (<= 4000000) fibs', even x]
  where
    fibs' = 1 : 1 : zipWith (+) fibs' (tail fibs')
