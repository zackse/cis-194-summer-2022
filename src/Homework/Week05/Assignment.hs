module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..),
  -- uncomment these once you've defined them:
  -- Expr(..),
  -- MinMax(..),
  -- Mod7(..)
) where

import Homework.Week05.ExprT
import Homework.Week05.Parser

import Data.Maybe

-- #1
eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- #2
evalStr :: String -> Maybe Integer
evalStr x
  | isNothing s = Nothing
  | otherwise   = Just . eval $ fromJust s
  where s = parseExp Lit Add Mul x

-- #3
-- class Expr a where
--   lit :: ???
--   add :: ???
--   mul :: ???

-- #4
-- instance Expr Integer where
--   lit = ???
--   add = ???
--   mul = ???
