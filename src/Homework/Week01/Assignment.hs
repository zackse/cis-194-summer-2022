module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)
--  | n < 10    = [n]
--  | otherwise = toDigits (n `div` 10) ++ [(n `mod` 10)]

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | n < 10    = [n]
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n  = reverse (doubleEveryOther' (reverse n))

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' []       = []
doubleEveryOther' (x:[])   = [x]
doubleEveryOther' (x:y:zs) = [x, y*2] ++ doubleEveryOther' zs

-- #3
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

-- #4
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
