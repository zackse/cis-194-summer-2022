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
parseWords [] = Unknown []
parseWords ("I" : t : xs)     = LogMessage Info               (read t) (unwords xs)
parseWords ("W" : t : xs)     = LogMessage Warning            (read t) (unwords xs)
parseWords ("E" : e : t : xs) = LogMessage ((Error . read) e) (read t) (unwords xs)
parseWords s                  = (Unknown . unwords) s

-- #1b
parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

--parse = parseLines . lines
--parseLines :: [String] -> [LogMessage]
--parseLines []     = []
--parseLines (x:xs) = parseMessage x : parseLines xs

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert = undefined

-- #3
build :: [LogMessage] -> MessageTree
build = undefined

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder = undefined

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
