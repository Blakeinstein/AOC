{-# LANGUAGE FlexibleContexts #-}
module Day23 (solve) where

import qualified Data.Array.Unboxed as Ub
import qualified Data.Set as S
import qualified Data.Map as M

import Coord (Coord(..), dirs, north, south, east, west)

type Hikes = Ub.UArray Coord Char

type State = (Coord, S.Set Coord)

type Inp = Hikes

slopes = M.fromList [('^', north), ('v', south), ('>', east), ('<', west)]

neighbours :: Hikes -> Bool -> State -> [State]
neighbours hikes hasSlopes (pos, path) = nextStates
  where
    possDest
      | hasSlopes && hikes Ub.! pos `elem` "^v<>" = [slopes M.! (hikes Ub.! pos)]
      | otherwise = dirs
    nextStates =
      filter (\(p, _) -> S.notMember p path && Ub.inRange b p && hikes Ub.! p /= '#') .
      map (\d -> (pos + d, S.insert (pos + d) path)) $
      possDest
    b = Ub.bounds hikes

explore :: Hikes -> Bool -> Coord -> State -> Int
explore hikes hasSlopes goal state@(pos, path)
  | pos == goal = S.size path - 1
  | null nextPos = negate 1
  | otherwise = maximum . map (explore hikes hasSlopes goal) $ nextPos
  where
    nextPos = neighbours hikes hasSlopes state

part1 :: Inp -> Int
part1 hikes = explore hikes True goalPos startState
  where 
    (_, Coord dy _) = Ub.bounds hikes
    posPos = Ub.indices hikes
    startPos =
      head . filter (\pos@(Coord y _) -> y == 0 && hikes Ub.! pos == '.') $ posPos
    goalPos =
      head . filter (\pos@(Coord y _) -> y == dy && hikes Ub.! pos == '.') $ posPos
    isGoal pos = fst pos == goalPos
    allSpots = length . filter (/= '#') . Ub.elems $ hikes
    startState = (startPos, S.singleton startPos)

part2 :: Inp -> Int
part2 hikes = explore hikes False goalPos startState
  where 
    (_, Coord dy _) = Ub.bounds hikes
    posPos = Ub.indices hikes
    startPos =
      head . filter (\pos@(Coord y _) -> y == 0 && hikes Ub.! pos == '.') $ posPos
    goalPos =
      head . filter (\pos@(Coord y _) -> y == dy && hikes Ub.! pos == '.') $ posPos
    isGoal pos = fst pos == goalPos
    allSpots = length . filter (/= '#') . Ub.elems $ hikes
    startState = (startPos, S.singleton startPos)

make2DArray :: Ub.IArray Ub.UArray a => [[a]] -> Ub.UArray Coord a
make2DArray l =
  Ub.array
    (Coord 0 0, Coord width height)
    [(Coord y x, l !! y !! x) | x <- [0 .. width], y <- [0 .. height]]
  where
    width = length (head l) - 1
    height = length l - 1

solve :: String -> IO ()
solve input = putStrLn "--- Day 23 ---" >> print (part1 p) >> print (part2 p)
  where
    p = make2DArray (lines input)
