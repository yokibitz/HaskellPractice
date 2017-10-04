isIntegralPalindrome :: Integer -> Bool
isIntegralPalindrome x = x == (read $ reverse $ show x :: Integer)

problem_4 = head [x | x <- [998001,998000..10000], isIntegralPalindrome x]