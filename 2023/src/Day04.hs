{-# LANGUAGE DeriveFunctor #-}
module Day04 (solve) where

import Control.Applicative (Alternative, asum, many, some)
import Control.Monad (void)

import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Regex.Applicative (RE, sym, string, psym, (=~))
import Text.Regex.Applicative.Common (decimal)
import Debug.Trace (trace)

type IntSet = S.Set Int
data Card a = Card Int a a deriving (Show, Functor)
type Inp = [Card IntSet]

type Parser t = RE Char t

spaces :: Parser ()
spaces = void (some (sym ' '))

intSet :: Parser IntSet
intSet = S.fromList <$> some number
  where number = spaces *> decimal

line :: Parser (Card IntSet)
line = Card <$> (string "Card" *> spaces *> decimal) <* string ":" <*> intSet <* string " |" <*> intSet

dbg :: Int -> Int
dbg a = trace (show a) a

part1 :: Inp -> Int
part1 = sum . map value
  where value (Card _ a b) | S.null winners = 0
                           | otherwise = 2 ^ (S.size winners - 1)
          where winners = S.intersection a b

part2 :: Inp -> Int
part2 cards = M.size netCards + sum (M.elems netCards)
  where netCards = M.fromList entries
        entries = do
          Card id win ours <- cards
          let winners = S.size $ S.intersection win ours
          pure (id, winners + sum [netCards M.! (id + i) | i <- [1..winners]])

solve :: String -> IO ()
solve input = putStrLn "--- Day 04 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = mapMaybe (=~ line) . lines
