module Day14 (solve) where

import Control.Arrow ((&&&))
import Control.Applicative (many)

import Data.Maybe (mapMaybe)
import Data.List (transpose, sortBy, elemIndex)
import Data.Ord (comparing, Down(..))

import Text.Regex.Applicative ( RE, sym, (=~), (<|>), string )
import Debug.Trace (trace)

dbg :: Show a => a -> a
dbg a = trace (show a) a

type Parser a = RE Char a

data Space = Empty | RoundRock | CubeRock deriving (Show, Eq, Ord)
type Inp = [[Space]]

load :: Inp -> Int
load = sum . map individual . withIndex
  where withIndex inp = zip [1..] (reverse inp)
        individual (i, row) = i * length (filter (== RoundRock) row)

tiltNorth :: Inp -> Inp
tiltNorth = transpose . map shift . transpose

shift :: [Space] -> [Space]
shift [] = []
shift s = let (a, b) = span (/= CubeRock) s
              (c, d) = break (/= CubeRock) b
          in sortBy (comparing Down) a ++ c ++ shift d

part1 :: Inp -> Int
part1 = load . tiltNorth

tiltWest, tiltEast, tiltSouth :: Inp -> Inp
tiltWest = map shift
tiltEast = map ((reverse . shift) . reverse)
tiltSouth = transpose . map ((reverse . shift) . reverse) . transpose

cycleOnce, cycle1B :: Inp -> Inp
cycleOnce = tiltEast . tiltSouth . tiltWest . tiltNorth
cycle1B xs = go 1000000000 [(load xs, xs)] xs
  where go n prev xs =
          let key = (load &&& id) $ cycleOnce xs in case elemIndex key prev of
            Nothing -> go n (key : prev) (snd key)
            Just i ->
              let cycleLength = i + 1
                  offset = length prev - (i + 1)
                  remain = (n - offset) `mod` cycleLength
              in (\(h:_) -> snd h) $ drop remain $ drop offset $ reverse prev

part2 :: Inp -> Int
part2 = load . cycle1B

parse :: Parser [Space]
parse = many $ (Empty <$ sym '.') <|> (RoundRock <$ sym 'O') <|> (CubeRock <$ sym '#')

solve :: String -> IO ()
solve input = putStrLn "--- Day 14 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = mapMaybe (=~ parse ) . lines
