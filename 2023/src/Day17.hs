module Day17 (solve) where

import qualified Data.Map as M
import Data.Char (digitToInt)
import Data.PSQueue (Binding ((:->)))
import qualified Data.PSQueue as PQ
import Data.List (foldl', (\\))

import Coord (Coord(..), Dir(..), (#+), distance, north, south, east, west)

import Debug.Trace (trace)
dbg :: Show a => a -> a
dbg a = trace (show a) a

type Prio = (Int, Int, Int)
type State = (Coord, (Int, Dir))

type CostMap = M.Map Coord Int

opposite :: Dir -> Dir
opposite U = D
opposite D = U
opposite L = R
opposite R = L
opposite None = None

shortestPath :: CostMap -> Coord -> (State -> Bool) -> (State -> [Dir]) -> (Coord -> Int) -> Int
shortestPath m start isFinished getNextDirs heuristic =
    let initialH = heuristic start
    in sp (PQ.singleton (start, (0, None)) (initialH, 0, initialH)) M.empty
  where
    sp :: PQ.PSQ State Prio -> M.Map State Int -> Int
    sp open closed =
      case PQ.minView open of
        Nothing -> error "No path found"
        Just (state :-> (f, g, h), restOpen) ->
            if isFinished state
            then g
            else let neighbors = map (posDirToBinding g) . filter (`M.notMember` closed) . filter ((`M.member` m) . fst) $ getNext state
                     newOpen = foldl' updateQueue restOpen neighbors
                in sp newOpen (M.insert state g closed)

    getNext :: State -> [State]
    getNext state@(coord, (n, dir)) = map f . getNextDirs $ state
      where
        f :: Dir -> State
        f d | d == dir = (coord #+ d, (n + 1, d))
            | otherwise = (coord #+ d, (1, d))

    updateQueue :: PQ.PSQ State Prio -> PQ.Binding State Prio -> PQ.PSQ State Prio
    updateQueue pq (key :-> prio) = PQ.alter (updateBinding prio) key pq

    updateBinding :: Prio -> Maybe Prio -> Maybe Prio
    updateBinding prio Nothing = Just prio
    updateBinding prio@(f, g, h) oldPrio@(Just (oldF, oldG, oldH)) = if g < oldG then Just prio
                                                                     else oldPrio 

    posDirToBinding :: Int -> State -> PQ.Binding State Prio
    posDirToBinding g pd@(coord, (n, dir)) = pd :-> (g + cost + h, g + cost, h)
      where
        cost = m M.! coord
        h = heuristic coord

part1 :: CostMap -> Int
part1 m = shortestPath m (Coord 0 0) finishRule getNextDirs heuristic
  where finish :: Coord
        (finish, _) = M.findMax m

        heuristic :: Coord -> Int
        heuristic = distance finish

        finishRule :: State -> Bool
        finishRule (c, _) = c == finish

        getNextDirs :: State -> [Dir]
        getNextDirs (_, (n, dir)) | n >= 3 = [U, R, D, L] \\ [opposite dir, dir]
                                  | otherwise = [U, R, D, L] \\ [opposite dir]

part2 :: CostMap -> Int
part2 m = shortestPath m (Coord 0 0) finishRule getNextDirs heuristic
  where finish :: Coord
        (finish, _) = M.findMax m

        heuristic :: Coord -> Int
        heuristic = distance finish

        finishRule :: State -> Bool
        finishRule (c, (n, _)) = c == finish && n >= 4

        getNextDirs :: State -> [Dir]
        getNextDirs (_, (n, dir)) | n == 0 = [U, R, D, L]
                                  | n < 4 = [dir]
                                  | n >= 10 = [U, R, D, L] \\ [opposite dir, dir]
                                  | otherwise = [U, R, D, L] \\ [opposite dir]

p :: String -> CostMap
p = M.fromList . concat . zipWith (\y -> zipWith (\x v -> (Coord y x, v)) [0..]) [0..] . map (map digitToInt) . lines

solve :: String -> IO ()
solve input = putStrLn "--- Day 17 ---" >> print (part1 $ p input) >> print (part2 $ p input)
