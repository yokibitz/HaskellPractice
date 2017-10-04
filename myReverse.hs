myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:a) = myReverse a ++ [x]


