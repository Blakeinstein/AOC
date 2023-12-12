{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Day10 (solve) where

import Control.Applicative (Alternative, asum, many, some)

import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Regex.Applicative (RE, sym, string, psym, (=~))
import Text.Regex.Applicative.Common (decimal)
import Debug.Trace (trace)
import Data.Tree (flatten)
import Data.List (nub)

import Coord (Coord(..), cardinal, north, east, south, west, invert, invert')

data Pipe = H | V | L | F | Se | St | J | G deriving (Show, Eq)

type Map = M.Map Coord Pipe

type Inp = Map

dbg :: Show a => a -> a
dbg a = trace (show a) a

type Parser a = RE Char a

dfsN :: Ord a => (a -> [a]) -> [a] -> [a]
dfsN = dfsOnN id

dfsOn ::
  Ord r =>
  (a -> r)    ->
  (a -> [a])  ->
  a           ->
  [a]
dfsOn rep next start = dfsOnN rep next [start]

dfsOnN ::
  Ord r =>
  (a -> r)    ->
  (a -> [a])  ->
  [a]         ->
  [a]
dfsOnN rep next = loop S.empty
  where
    loop !seen = \case
      [] -> []
      x:xs
        | S.member r seen ->     loop seen xs
        | otherwise         -> x : loop seen1 (next x ++ xs)
        where
          r     = rep x
          seen1 = S.insert r seen

turnRight :: Coord -> Coord
turnRight (Coord y x) = Coord x (-y)

pickStart :: Inp -> (Coord, Coord)
pickStart inp = head $
  [ (k, dir)
  | (k, St)  <- M.assocs inp
  , (dir, ok) <- [(south, [L, V, J]), (north, [F, V, Se]), (west, [Se, H, J])]
  , let next = M.findWithDefault G (k + dir) inp
  , next `elem` ok
  ]

pipeEffect :: Pipe -> Coord -> Coord
pipeEffect = \case
  St -> id
  H -> id
  V -> id
  Se -> invert
  L -> invert
  J -> invert'
  F -> invert'
  _   -> error "bad pipe character"

step :: Map -> (Coord, Coord) -> [(Coord, Coord)]
step inp (dir, here) =
  [(dir', here + dir') | let dir' = pipeEffect (inp M.! here) dir]

part1 :: Inp -> Int
part1 inp = let (start, dir0) = pickStart inp
                route = [(here, dir) | (dir, here) <- dfsOn snd (step inp) (dir0, dir0 + start)]
                in length route `quot` 2

openNeighbors :: S.Set Coord -> Coord -> [Coord]
openNeighbors input x = [y | y <- cardinal x, S.member y input]

rightof :: Map -> (Coord, Coord) -> [Coord]
rightof input (here, dir) =
  nub [turnRight d + here | d <- [dir, pipeEffect (input M.! here) dir]]


part2 :: Inp -> Int
part2 inp = let (start, dir0) = pickStart inp
                route = [(here, dir) | (dir, here) <- dfsOn snd (step inp) (dir0, dir0 + start)]
                pipe = S.fromList (map fst route)
                containable = M.keysSet inp `S.difference` pipe
                candidates = S.fromList (concatMap (rightof inp) route) `S.difference` pipe
                contained = dfsN (openNeighbors containable) (S.toList candidates)
                in length contained

pipe :: Parser Pipe
pipe = asum [ H <$ sym '-', V <$ sym '|', L <$ sym 'L', F <$ sym 'F', Se <$ sym '7', St <$ sym 'S', J <$ sym 'J', G <$ sym '.']

parse :: Parser [Pipe]
parse = many pipe

toMap :: [[Pipe]] -> Inp
toMap lines =  M.fromList [(Coord y x, pipe) | (y, line) <- zip [0..] lines, (x, pipe) <- zip [0..] line]

solve :: String -> IO ()
solve input = putStrLn "--- Day 10 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = toMap . mapMaybe (=~ parse) . lines
