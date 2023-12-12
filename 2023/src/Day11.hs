module Day11 (solve) where

import Control.Applicative (many)
import Control.Monad (guard)

import Data.Maybe (mapMaybe)
import Data.List (tails, transpose)
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Regex.Applicative (RE, (=~), sym, (<|>))
import Debug.Trace (trace)

import Coord (Coord(..), distance)
import Data.Ix (Ix(inRange))

data Space = Empty | Galaxy deriving (Show, Eq)
type Map = M.Map Coord Space
type Gaps = [Int]

dbg :: Show a => String -> a -> a
dbg msg a = trace (msg ++ ": " ++ show a) a

dbgf :: (Show a, Show b) => (a -> b) -> a -> b
dbgf f a = dbg "Result value" $ f (dbg "Input value" a)

type Parser a = RE Char a

type Inp = [[Space]]


findGalaxies :: Inp -> [Coord]
findGalaxies inp = [Coord y x
  | (y, row) <- zip [0..] inp
  , (x, tile) <- zip [0..] row
  , tile == Galaxy
  ]

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

findGaps' :: Inp -> Gaps
findGaps' inp = [x | (x, row) <- zip [0..] inp, all (== Empty) row]

findGaps :: Inp -> (Gaps, Gaps)
findGaps inp = (x, y)
  where y = findGaps' inp
        x = findGaps' (transpose inp)

countGap :: Gaps -> Int -> Int -> Int -> Int
countGap g m a b = (length . filter (inRange (start, end)) $ g) * (m-1)
  where start = min a b
        end = max a b

padded :: (Gaps, Gaps) -> Int -> (Coord, Coord) -> Int
padded (gX, gY) m (c1@(Coord y1 x1), c2@(Coord y2 x2)) = dist + px + py
  where dist = distance c1 c2
        px = countGap gX m x1 x2
        py = countGap gY m y1 y2

part1 :: Inp -> Int
part1 inp =sum  $ map (padded gaps 2) galaxies
  where galaxies = pairs . findGalaxies $ inp
        gaps = findGaps inp

part2 :: Inp -> Int
part2 inp = sum  $ map (padded gaps 1000000) galaxies
  where galaxies = pairs . findGalaxies $ inp
        gaps = findGaps inp

space :: Parser Space
space = Empty <$ sym '.' <|> Galaxy <$ sym '#'

parse :: Parser [Space]
parse = many space

solve :: String -> IO ()
solve input = putStrLn "--- Day 11 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = mapMaybe (=~ parse) . lines
