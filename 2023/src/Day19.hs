module Day19 (solve) where

part1 :: [String] -> Int
part1 [] = 0

part2 :: [String] -> Int
part2 [_] = 0

solve :: String -> IO ()
solve input = putStrLn "--- Day 01 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = map read . lines
