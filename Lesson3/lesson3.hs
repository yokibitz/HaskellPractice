data IntList = Empty | Cons Int IntList
    deriving Show

exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

addOne x = x + 1
square x  = x * x

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList op (Cons x xs) = Cons (op x) $ mapIntList op xs

-- mapIntList addOne exampleList
-- mapIntList abs exampleList
-- mapIntList square exampleList

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList op (Cons x xs)
    | op x      = Cons x $ filterIntList op xs
    | otherwise = filterIntList op xs

foldIntList :: (Int -> Int -> Int) -> IntList -> Int
foldIntList _ Empty = 0
foldIntList op (Cons x xs) = op x $ foldIntList op xs

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
    | even x    = Cons x $ keepOnlyEven xs
    | otherwise = keepOnlyEven xs

keepOnlyPositive :: IntList -> IntList
keepOnlyPositive Empty = Empty
keepOnlyPositive (Cons x xs)
    | x >= 0    = Cons x $ keepOnlyPositive xs
    | otherwise = keepOnlyPositive xs

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) $ absAll xs

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x*x) $ squareAll xs

sumIntList :: IntList -> Int
sumIntList Empty = 0
sumIntList (Cons x xs) = x + sumIntList xs

data List t = E | C t (List t)
    deriving Show

filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList op (C x xs)
    | op x      = Cons x $ filterList xs

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)