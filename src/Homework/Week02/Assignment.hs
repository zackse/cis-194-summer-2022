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
--data MessageTree = Leaf
--                 | Node MessageTree LogMessage MessageTree
--  deriving (Show, Eq)
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert l@(LogMessage _ _ _) Leaf = Node Leaf l Leaf
insert l@(LogMessage _ ts _) mt@(Node tl m@(LogMessage _ mts _) tr) = if ts < mts
                                                                      then Node (insert l tl) m tr
                                                                      else Node tl m (insert l tr)

-- #3
build :: [LogMessage] -> MessageTree
build = undefined

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder = undefined

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
