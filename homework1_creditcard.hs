doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x : 2*y : doubleEveryOther zs

doubleBeforeLast :: [Integer] -> [Integer]
doubleBeforeLast l = reverse (doubleEveryOther (reverse l))

toDigits :: Integer -> [Integer]
toDigits n
    | n == 0 || n < 0   = []
    | otherwise         =
         toDigits (n `div` 10) ++ [n `mod` 10]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:xs) = sumDigits(toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x
    | length (toDigits x) /= 16 = False
    | otherwise                 =
        sumDigits (doubleBeforeLast (toDigits x)) `mod` 10 == 0 
