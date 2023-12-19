{-# LANGUAGE OverloadedStrings #-}
module Day13 (solve) where

import Control.Applicative ( Alternative, many, some )

import Data.Maybe (fromMaybe, fromJust)
import Data.List (transpose, find)

import Text.Regex.Applicative ( RE, sym, (=~), (<|>), string )
import Debug.Trace (trace)

sepBy :: Alternative f => f a -> f b -> f [a]
p `sepBy` sep = (:) <$> p <*> many (sep *> p)

dbg :: Show a => a -> a
dbg a = trace (show a) a

type Parser a = RE Char a

data Node = Ash | Rock deriving (Show, Eq, Ord)
type Map = [[Node]]

type Inp = [Map]

diff :: Map -> Map -> Int
diff pat = sum . zipWith f pat
  where f r = sum . zipWith (\c1 c2 -> if c1 == c2 then 0 else 1) r

reflection :: Int -> Map -> Maybe Int
reflection delta pat = find f [1..length pat - 1]
  where f row = let (a, b) = splitAt row pat in diff (reverse a) b == delta

summarize :: Int -> Map -> Int
summarize delta = fromJust . f
  where f pat = (*100) <$> reflection delta pat <|> reflection delta (transpose pat)

summarizeAll :: Int -> Inp -> Int
summarizeAll delta = sum . map (summarize delta)

part1 :: Inp -> Int
part1 = summarizeAll 0

part2 :: Inp -> Int
part2 = summarizeAll 1

node :: Parser Node
node = (Ash <$ ".") <|> (Rock <$ "#")

nodeLine :: Parser [Node]
nodeLine = some node <* "\n"

parseMap :: Parser Map
parseMap = some nodeLine

parse :: Parser Inp
parse = parseMap `sepBy` "\n"

solve :: String -> IO ()
solve input = putStrLn "--- Day 13 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = fromMaybe (error "No Parse") . (=~ parse)

