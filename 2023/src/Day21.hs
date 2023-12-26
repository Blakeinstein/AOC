{-# LANGUAGE FlexibleContexts #-}
module Day21 (solve) where
  

import Control.Applicative (many)

import Data.Maybe (mapMaybe)
import Data.Array.IArray (IArray, Ix)
import Data.Array.Unboxed (UArray, inRange, array, bounds, indices, (!))
import Data.Graph (graphFromEdges)
import qualified Data.List as L
import Data.List.Split (chunksOf)
import qualified Data.Sequence as Sq (Seq ((:<|), (:|>)), null, singleton)
import qualified Data.Set as S

import Linear.V2          (V2 (..))


type Pos = V2 Int

north, south, east, west :: Pos
north = V2 0 (-1)
south = V2 0 1
east = V2 1 0
west = V2 (-1) 0

dirs = [north, south, east, west]

neighbours :: (IArray UArray a) => UArray Pos a -> Pos -> [Pos]
neighbours a p = filter (inRange b) . map (p +) $ dirs
  where
    b = bounds a

type Garden = UArray Pos Char

type Inp = (Garden, Pos)

reachable :: Garden -> Pos -> S.Set Pos
reachable garden =
  S.fromList . L.filter ((/= '#') . (!) garden) . neighbours garden

reachBool :: Garden -> Pos -> Bool
reachBool garden = not . S.null . reachable garden

doStep :: Garden -> (S.Set Pos, S.Set Pos) -> (S.Set Pos, S.Set Pos)
doStep garden (cur, seen) =
  ( S.unions . S.map (S.filter (`S.notMember` newSeen) . reachable garden) $ cur
  , newSeen)
  where
    newSeen = cur `S.union` seen

setReach :: Garden -> Pos -> Int -> S.Set Pos
setReach garden startPos steps =
  S.filter ((== steps `mod` 2) . vPar) .
  S.unions . L.map (fst . (!!) allSteps) . takeWhile (<= steps) $
  [0 ..]
  where
    allSteps =
      take (steps + 1) . iterate (doStep garden) $ (S.fromList [startPos], S.empty)
    vPar (V2 x y) = (x + y) `mod` 2

part1 :: Inp -> Int
part1 (garden, startPos) = S.size . setReach garden startPos $ steps
  where steps = 64

infiniteReach :: Garden -> Pos -> S.Set Pos
infiniteReach garden pos =
  S.fromList . L.filter ((/= '#') . (!) garden . vecMod) . L.map (pos +) $ dirs
  where
    (_, V2 mx _) = bounds garden
    w = mx + 1
    vecMod (V2 x y) = V2 (x `mod` w) (y `mod` w)

infiniteStep :: Garden -> (S.Set Pos, S.Set Pos) -> (S.Set Pos, S.Set Pos)
infiniteStep garden (cur, seen) =
  ( S.unions . S.map (S.filter (`S.notMember` newSeen) . infiniteReach garden) $
    cur
  , newSeen)
  where
    newSeen = cur `S.union` seen

setInfiniteReach :: Garden -> Pos -> Int -> S.Set Pos
setInfiniteReach garden startPos steps =
  S.filter ((== steps `mod` 2) . vPar) .
  S.unions . L.map (fst . (!!) allSteps) . takeWhile (<= steps) $
  [0 ..]
  where
    allSteps =
      take (steps + 1) . iterate (infiniteStep garden) $
      (S.fromList [startPos], S.empty)
    vPar (V2 x y) = (x + y) `mod` 2

part2 :: Inp -> Int
part2 (garden, startPos@(V2 sx sy)) = a * (divisor ^ 2) + b * divisor + zero
  where (_, V2 mx my) = bounds garden
        w = mx + 1
        remainder = mod steps w
        divisor = div steps w
        zero = S.size . setInfiniteReach garden startPos $ remainder
        one = S.size . setInfiniteReach garden startPos $ w + remainder
        two = S.size . setInfiniteReach garden startPos $ 2 * w + remainder
        a = div (two + c - 2 * one) 2
        b = one - c - a
        c = zero
        three
          | odd steps =
            S.size . setInfiniteReach garden startPos $ 3 * w + remainder
          | even steps =
            S.size . setInfiniteReach garden startPos $ 3 * w + remainder
        predictThree = 9 * a + 3 * b + c
        steps = 26501365

make2DArray :: IArray UArray a => [[a]] -> UArray (V2 Int) a
make2DArray l =
  array
    (V2 0 0, V2 width height)
    [(V2 x y, l !! y !! x) | x <- [0 .. width], y <- [0 .. height]]
  where
    width = length (head l) - 1
    height = length l - 1

solve :: String -> IO ()
solve input = putStrLn "--- Day 01 ---" >> print (part1 (garden, startPos)) >> print (part2 (garden, startPos))
  where
    garden = make2DArray ( lines input )
    startPos = head . L.filter ((== 'S') . (!) garden) . indices $ garden
