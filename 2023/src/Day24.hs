module Day24 (solve) where

import Control.Monad (void)
import Control.Applicative (some)

import Data.Maybe (mapMaybe)

import Text.Regex.Applicative ( RE, sym, (=~), (<|>), string, psym )
import Text.Regex.Applicative.Common (decimal, signed)

import Linear.V3 (V3 (..))

import Debug.Trace (trace)
dbg :: Show a => a -> a
dbg a = trace (show a) a

type Pos = V3 Int
type Velocity = V3 Int
type Hailstone = (Pos, Velocity)

type Parser a = RE Char a
type Inp = [Hailstone]

spaces :: Parser ()
spaces = void (some (sym ' '))

intersect :: Hailstone -> Hailstone -> Bool
intersect ((V3 x1 y1 _), (V3 dx1 dy1 _)) ((V3 x2 y2 _), (V3 dx2 dy2 _)) =
    let xdiff = (-dx1, -dx2)
        ydiff = (-dy1, -dy2)
        det (a, b) = fst a * snd b - snd a * fst b
        div' = det (xdiff, ydiff)
        d = (det ((x1, y1), (x1 + dx1, y1 + dy1)), det ((x2, y2), (x2 + dx2, y2 + dy2)))
        x = det (d, xdiff) `div` div'
        y = det (d, ydiff) `div` div'
    in  div' /= 0
        && 200000000000000 <= x && x <= 400000000000000 && 200000000000000 <= y && y <= 400000000000000
        && (x - x1 > 0) == (dx1 > 0) && (y - y1 > 0) == (dy1 > 0)
        && (x - x2 > 0) == (dx2 > 0) && (y - y2 > 0) == (dy2 > 0)

combinations2 :: [a] -> [(a, a)]
combinations2 [] = []
combinations2 (x:xs) = map ((,) x) xs ++ combinations2 xs

part1 :: Inp -> Int
part1 = length . filter (uncurry intersect) . combinations2 

part2 :: Inp -> Int
part2 = length

number :: Parser Int
number = signed decimal

triple :: Parser Pos
triple = V3 <$> number <* sym ',' <* spaces <*> number <* sym ',' <* spaces <*> number

parse :: Parser Hailstone
parse = (,) <$> triple <* string " @" <* spaces <*> triple

solve :: String -> IO ()
solve input = putStrLn "--- Day 24 ---" >> print (part1 p) >> print (part2 p)
  where
    p = mapMaybe (=~ parse) . lines $ input
