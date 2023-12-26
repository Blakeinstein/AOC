{-# LANGUAGE TupleSections #-}
module Day25 (solve) where

import qualified Data.Map as M
import Data.Graph.Inductive.Graph (Graph (..), Node, mkGraph, Path, insEdge, delEdge)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.BFS (bft)
import Data.Graph.Inductive.Query.DFS (scc)
import Data.List (nub, maximumBy)

type Diagram = Gr String String

type Inp = Diagram

dicToGraph :: M.Map String [(String, a)] -> Gr String a
dicToGraph dic = mkGraph nodes edges
  where
    nodes = zip [0 ..] . nub $ M.keys dic ++ map fst (concat . M.elems $ dic)
    edges =
      map (\(a, (b, c)) -> (labelToNode M.! a, labelToNode M.! b, c)) .
      concatMap (\(a, b) -> map (a, ) b) . M.assocs $
      dic
    labelToNode = M.fromList . map (\(a, b) -> (b, a)) $ nodes

findFurthest :: Node -> Diagram -> Path
findFurthest node = maximumBy (\a b -> compare (length a) (length b)) . bft node

digraph :: Diagram -> Diagram
digraph diagram =
  foldl (\a (b, c, d) -> insEdge (c, b, d) a) diagram . labEdges $ diagram

cutPath :: Path -> Diagram -> Diagram
cutPath [a] d      = d
cutPath (a:b:as) d = cutPath (b : as) . delEdge (a, b) . delEdge (b, a) $ d

cutDiagram :: Diagram -> Diagram
cutDiagram diagram = thirdCut
  where
    furPath = findFurthest 0 . digraph $ diagram
    firstCut = cutPath furPath . digraph $ diagram
    secondCut = cutPath (findFurthest 1 firstCut) firstCut
    thirdCut = cutPath (findFurthest 2 secondCut) secondCut

part1 :: Inp -> Int
part1 = product .
  map length .
  scc .
  cutDiagram

solve :: String -> IO ()
solve input = putStrLn "--- Day 25 ---" >> print (part1 graph)
  where
    graph = 
      dicToGraph .
      M.fromList .
      map ((\(a:b) -> (init a, map (\c -> (c, init a ++ " " ++ c)) b)) . words) .
      lines $ input
