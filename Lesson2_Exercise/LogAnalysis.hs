{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage "" = Unknown []
parseMessage input = 
    case words input of
    ("I":x:ys)      ->  LogMessage Info (read x) (unwords ys)
    ("W":x:ys)      ->  LogMessage Warning (read x) (unwords ys)
    ("E":x:y:zs)    ->  LogMessage (Error $ read x) (read y) (unwords zs)
    x               ->  Unknown $ unwords x

parse :: String -> [LogMessage]
parse input = map parseMessage $ lines input

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) y = y
insert message Leaf = Node Leaf message Leaf
insert message@(LogMessage _ ts _) (Node l m@(LogMessage _ mts _) r)
    | ts < mts  = Node (insert message l) m r 
    | ts > mts  = Node l m (insert message r)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build [x] = insert x Leaf
build (x:xs) = insert x $ build xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong xs = [m | LogMessage (Error severity) _ m <- inOrder (build xs), severity >= 50]

-- test = Node (Node Leaf (parseMessage "I 1 Info message") Leaf) (parseMessage "E 100 2 Error Message") Leaf