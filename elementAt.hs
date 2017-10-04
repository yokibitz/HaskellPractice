elementAt :: [a] -> Int -> a
elementAt xs a = last (take a xs)