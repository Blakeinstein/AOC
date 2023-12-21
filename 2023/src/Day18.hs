module Day18 (solve) where

import Control.Applicative (many)

import Data.Maybe (mapMaybe)

import Text.Regex.Applicative ( RE, sym, (=~), (<|>), string, psym )
import Text.Regex.Applicative.Common (decimal)

import Coord (Coord(..), north, south, east, west)

import Debug.Trace (trace)
dbg :: Show a => a -> a
dbg a = trace (show a) a

data Ins = Ins Coord Int deriving (Show, Eq, Ord)
type State = (Coord, Int)

type Parser a = RE Char a

type Inp = ([Ins], [Ins])

area :: [Ins] -> Int
area steps = abs (snd (foldl move (Coord 0 0, 2) steps)) `div` 2
  where
    move :: State -> Ins -> State
    move (coord@(Coord py px), s) (Ins dir count) =
        let cnext@(Coord y x) = coord + (dir * Coord count count)
        in (cnext, s + count + (py + y) * (px - x))

part1 :: Inp -> Int
part1 (invalid, _)= area invalid

part2 :: Inp -> Int
part2 (_, valid) = area valid

dir :: Parser Coord
dir = (north <$ sym 'U') <|> (east <$ sym 'R') <|> (south <$ sym 'D') <|> (west <$ sym 'L')

fromLine :: Coord -> Int -> String -> (Ins, Ins)
fromLine dir count color = (Ins dir count, decode color)
  where decode :: String -> Ins
        decode ['#', a, b, c, d, e, f] = Ins (toDir f) (read ("0x" ++ [a, b, c, d, e]) :: Int)
          where toDir :: Char -> Coord
                toDir '0' = north
                toDir '1' = east
                toDir '2' = south
                toDir '3' = west
                toDir _ = error "Invalid direction"

parse :: Parser (Ins, Ins)
parse = fromLine <$> (dir <* sym ' ') <*> decimal <*> (string " (" *> many (psym (/= ')')) <* sym ')')

solve :: String -> IO ()
solve input = putStrLn "--- Day 18 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = unzip . mapMaybe (=~ parse) . lines
