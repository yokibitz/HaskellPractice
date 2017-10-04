myLength :: [a] -> Int
myLength xs = sum [1 | _ <- xs]