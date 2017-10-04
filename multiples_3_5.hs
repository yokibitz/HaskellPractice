multiple :: (Integral a) => [a] -> ([a], a)
multiple xs = ([x | x <- xs, isMultipleOf3Or5 x], sum[x | x <- xs, isMultipleOf3Or5 x])
	where
		isMultipleOf3Or5 x = mod x 3 == 0 || mod x 5 == 0
		
-- multiple :: (Integral a) => [a] -> ([a], a)
-- multiple xs = (map (isMultipleOf3Or5) xs, sum (map (isMultipleOf3Or5) xs))
	-- where
		-- isMultipleOf3Or5 x = mod x 3 == 0 || mod x 5 == 0