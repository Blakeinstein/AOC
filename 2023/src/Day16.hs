

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
module Day16 (solve) where

import Control.Applicative ( many, some, liftA2 )

import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (transpose, sortBy, elemIndex)
import Data.Foldable (toList)
import Data.Ix (Ix, inRange)
import Data.Ord (comparing, Down(..))
import qualified Data.Array.IArray as A
import qualified Data.Set as S

import Text.Regex.Applicative ( RE, sym, (=~), (<|>), string )
import Debug.Trace (trace)

import Coord (Coord(..), north, south, east, west)
import Data.Bits (xor)

dbg :: Show a => a -> a
dbg a = trace (show a) a

type Parser a = RE Char a

data Direction = North | East | South | West deriving (Show, Eq, Ord, Enum)
data Some a = One a | Two a a deriving (Show, Functor, Foldable, Traversable)
data Feature = Empty | Mirror (Direction -> Direction) | Splitter (Direction -> Some Direction)
data Trace = Trace {_coord :: Coord, _dir :: Direction} deriving (Show, Eq, Ord)

unit :: Direction -> Coord
unit North = north
unit East = east
unit South = south
unit West = west

nexts :: Feature -> Direction -> Some Direction
nexts Empty = One
nexts (Mirror m) = One . m
nexts (Splitter s) = s

type Inp = A.Array Coord Feature

step :: Inp -> Trace -> Some Trace
step g (Trace c d) = go <$> nexts (g A.! c) d
  where go d'= Trace ((+) c (unit d')) d'

stepAll :: Inp -> [Trace] -> [Trace]
stepAll g = filter inBounds . (>>= (toList . step g))
  where inBounds (Trace c _) = inRange r c
        r = A.bounds g

energizedBy :: Inp -> Trace -> Int
energizedBy g = S.size . S.map _coord . go S.empty . pure
  where go seen ts = case filter (not . flip S.member seen) ts of
          [] -> seen
          xs -> go (S.union seen (S.fromList xs)) (stepAll g xs)

part1 :: Inp -> Int
part1 g = energizedBy g (Trace (Coord 0 0) East)

part2 :: Inp -> Int
part2 g = maximum . map (energizedBy g) $ starts
  where starts = concat [ [ Trace (Coord y 0) East | y <- [0..maxY]]
                        , [ Trace (Coord y maxX) West | y <- [0..maxY]]
                        , [ Trace (Coord 0 x) South | x <- [0..maxX]]
                        , [ Trace (Coord maxY x) North | x <- [0..maxX]]
                        ]
        (_, Coord maxY maxX) = A.bounds g

emptyF, mirror, splitter, feature :: Parser Feature
emptyF = Empty <$ sym '.'
mirror = Mirror . reflect <$> (1 <$ sym '/' <|> 3 <$ sym '\\')
  where reflect mask = toEnum . xor mask . fromEnum
splitter = Splitter . splitter' <$> ([West, East] <$ sym '-' <|> [North, South] <$ sym '|')
  where splitter' dirs d | d `elem` dirs = One d
                         | otherwise = Two (go (+ 1)) (go (subtract 1))
          where go f = toEnum $ f (fromEnum d) `mod` 4
feature = emptyF <|> mirror <|> splitter

line :: Parser [Feature]
line = some feature

parse :: Parser [[Feature]]
parse = some (line <* sym '\n')

solve :: String -> IO ()
solve input = putStrLn "--- Day 16 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p s = mkArray . fromMaybe (error "no parse") . (=~ parse) $ s
      where mkArray fss = A.listArray (Coord 0 0, Coord (h - 1) (w - 1)) $ concat fss
              where h = length fss
                    w = length (head fss)