fibonacci' n 
	| n <= 0 	= 0
	| n == 1	= 1
	| otherwise	= fibonacci' (n-1) + fibonacci' (n-2)

problem_2 = sum [ x | x <- takeWhile (<= 4000000) fibs, even x]
  where
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
-- evenSum = sum[x | x <- map (fibonacci') [1..], ]