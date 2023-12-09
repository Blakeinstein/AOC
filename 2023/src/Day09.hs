module Day09 (solve) where

import Control.Applicative (Alternative, many)

import Data.Maybe (mapMaybe)

import Text.Regex.Applicative (RE, sym, (=~))
import Text.Regex.Applicative.Common (decimal, signed)
import Debug.Trace (trace)

sepBy :: Alternative f => f a -> f b -> f [a]
p `sepBy` sep = (:) <$> p <*> many (sep *> p)

type Parser a = RE Char a

diff [] = []
diff ls = zipWith (-) (tail ls) ls

dbg :: Show a => a -> a
dbg a = trace (show a) a

predictNext :: [Int] -> Int
predictNext curr | allZeroes curr = 0
                 | otherwise = predictNext next + last curr
  where allZeroes = all (== 0)
        next = diff curr

part1 :: [[Int]] -> Int
part1 = sum . map predictNext

predictPrev :: [Int] -> Int
predictPrev curr | allZeroes curr = 0
                 | otherwise = head curr - predictPrev next
  where allZeroes = all (== 0)
        next = diff curr

part2 :: [[Int]] -> Int
part2 = sum . map predictPrev

parser :: Parser [Int]
parser = signed decimal `sepBy` sym ' '

solve :: String -> IO ()
solve input = putStrLn "--- Day 01 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = mapMaybe (=~ parser) . lines
