module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

-- #1
skips :: [a] -> [[a]]
skips [] = []
skips xs = map (\i -> everyNth i xs) [1..length xs]

-- TODO: use Control.Lens to avoid partial '!!' function
-- https://stackoverflow.com/a/23627631
everyNth :: Int -> [a] -> [a]
everyNth n xs = [xs !! (i-1) | i <- [1..length xs],
                               i `mod` n == 0]

nth :: Int -> [a] -> Maybe a
nth n xs = myhead $ take 1 (drop (n-1) xs)

myhead :: [a] -> Maybe a
myhead [] = Nothing
myhead (x:_) = Just x

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x:y:z:zs) | x < y && y > z = [y] ++ localMaxima ([y,z] ++ zs)
localMaxima (x:xs) = localMaxima xs

-- #3
histogram :: [Integer] -> String
histogram = undefined
