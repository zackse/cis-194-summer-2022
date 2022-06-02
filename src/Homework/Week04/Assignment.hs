module Homework.Week04.Assignment (
  ex1,
  ex2,
  ex3,
  ex4,
  ex5,
  ex6,
  ex7,
  ex8,
  ex9,
  ex10,
  ex11,
  ex12,
  insertBST,
  allCaps,
  dropTrailingWhitespace,
  firstLetters,
  asList,
  BST(..)
) where

import Homework.Week04.BST

import Data.Char
import Data.Maybe

-- #1
ex1 :: a -> b -> b
ex1 _ b = b

-- #2
ex2 :: a -> a -> a
ex2 a _ = a

-- #3
ex3 :: Int -> a -> a
ex3 _ a = a

-- #4
ex4 :: Bool -> a -> a -> a
ex4 _ a _ = a 

-- #5
ex5 :: Bool -> Bool
ex5 True = False
ex5 _ = True

-- #6
-- can't be defined?
ex6 :: (a -> a) -> a
ex6 = undefined

-- #7
ex7 :: (a -> a) -> a -> a
ex7 f a = f a

-- #8
ex8 :: [a] -> [a]
ex8 [] = []
ex8 (x:xs) = x:[]

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 f [] = []
ex9 f (x:xs) = f x : ex9 f xs

-- #10
-- can't be defined?
ex10 :: Maybe a -> a
ex10 = undefined

-- #11
ex11 :: a -> Maybe a
ex11 a = Just a

-- #12
ex12 :: Maybe a -> Maybe a
ex12 = undefined

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ new Leaf = Node Leaf new Leaf
insertBST f new (Node left old right)
  | o == LT = Node (insertBST f new left) old right
  | o == EQ = Node (insertBST f new left) old right
  | o == GT = Node left old (insertBST f new right)
  where o = f new old

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

-- #14
allCaps :: [String] -> Bool
allCaps [] = True
allCaps ("":_) = False
allCaps (x:xs) = isUpper (fromJust (safeHead x)) && allCaps xs

allCaps' :: [String] -> Bool
allCaps' xs = xs == filter (\a -> isUpper (fromMaybe 'a' (safeHead a))) xs

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace s = reverse . dropLeadingWhitespace $ reverse s

dropLeadingWhitespace :: String -> String
dropLeadingWhitespace [] = []
dropLeadingWhitespace (x:xs)
  | isSpace x = dropLeadingWhitespace xs 
  | otherwise = x:xs 

-- #16
firstLetters :: [String] -> [Char]
firstLetters xs = map (\a -> fromJust (safeHead a)) $ filter (\a -> isLetter (fromMaybe ' ' (safeHead a))) xs

-- #17
asList :: [String] -> String
asList xs = "[" ++ asList' xs ++ "]"

asList' :: [String] -> String
asList' [] = ""
asList' (x:[]) = x
asList' (x:xs) = x ++ "," ++ (asList' xs)
