data Thing  = Shoe
            | Ship
            | SealingWax
            | Cabbage
            | King
    deriving Show

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Shoe        = True
isSmall Ship        = False
isSmall SealingWax  = True
isSmall Cabbage     = True
isSmall King        = False

isSmall2 :: Thing -> Bool
isSmall2 King   = False
isSmall2 Ship   = False
isSmall2 _      = True

data FailableDouble = Failure
                    | OK Double
    deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure   = 0
failureToZero (OK n)    = n

data Person = Person String Int Thing
    deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

baz :: Person -> String
baz p@(Person a _ _) = "The name field of (" ++ show p ++ ") is " ++ a

checkFav :: Person -> String
checkFav (Person n _ Cabbage) = n ++ ", you're my kind of person!"
checkFav (Person n _ _) = n ++ ", you're favorite thing is lame!"

data IntList = Empty | Cons Int IntList
    deriving Show

intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x l) = x * intListProd l

data Tree   = Leaf Char
            | Node Tree Int Tree
    deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))