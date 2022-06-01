module Homework.Week02.Assignment (
  build,
  inOrder,
  insert,
  parse,
  parseMessage,
  whatWentWrong,
  LogMessage(..),
  MessageTree(..),
  MessageType(..),
  TimeStamp
) where

import Homework.Week02.Log

-- #1a
parseMessage :: String -> LogMessage
parseMessage = (parseWords . words)

parseWords :: [String] -> LogMessage
parseWords ("I" : t : xs)     = LogMessage Info               (read t) (unwords xs)
parseWords ("W" : t : xs)     = LogMessage Warning            (read t) (unwords xs)
parseWords ("E" : e : t : xs) = LogMessage ((Error . read) e) (read t) (unwords xs)
parseWords s                  = (Unknown . unwords) s

-- #1b
parse :: String -> [LogMessage]
parse = map parseMessage . lines

--parse = parseLines . lines
--parseLines :: [String] -> [LogMessage]
--parseLines []     = []
--parseLines (x:xs) = parseMessage x : parseLines xs

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert newmsg@(LogMessage _ _ _) Leaf = Node Leaf newmsg Leaf
insert newmsg@(LogMessage _ ts _) (Node left oldmsg@(LogMessage _ mts _) right)
  | ts < mts  = Node (insert newmsg left) oldmsg right
  | otherwise = Node left oldmsg (insert newmsg right)

-- #3
build :: [LogMessage] -> MessageTree
build xs = foldl (\tree msg -> insert msg tree) Leaf xs

--build xs = buildTree Leaf xs
--buildTree :: MessageTree -> [LogMessage] -> MessageTree
--buildTree tree []     = tree
--buildTree tree (x:xs) = buildTree (insert x tree) xs

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map (\(LogMessage _ _ s) -> s) . filter isRelevantErrorLog $ inOrder $ build xs

isRelevantErrorLog :: LogMessage -> Bool
isRelevantErrorLog (LogMessage (Error e) _ _) = e > 50
isRelevantErrorLog _ = False

-- doesn't work because lambda pattern is incomplete
--whatWentWrong xs = map (\(LogMessage _ _ s) -> s) $ filter (\(LogMessage (Error e) _ _) -> e > 50) $ inOrder $ build xs
