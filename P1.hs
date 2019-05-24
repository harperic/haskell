-- Sum of all of the multiple of 3 or 5 below 1000
-- This should be an easy one-liner

-- Don't use the union...although that's a fun thing
import Data.List (union)
problem_1' = sum (union [3,6..999] [5,10..999])

problem_1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
