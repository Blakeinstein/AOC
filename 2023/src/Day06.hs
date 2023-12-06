module Day06 (solve) where

import Control.Applicative (Alternative, asum, many, some)
import Control.Monad (void)

import Data.Char (isDigit)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Regex.Applicative (RE, sym, string, psym, (=~))
import Text.Regex.Applicative.Common (decimal)
import Debug.Trace (trace)

type Race = (Int, Int)
type Inp = [Race]

type Parser t = RE Char t

spaces :: Parser ()
spaces = void (some (sym ' '))

numbers :: Parser [Int]
numbers = some (spaces *> decimal)

parse1 :: Parser Inp
parse1 = zip <$> (string "Time:" *> numbers) <* string "\nDistance:" <*> numbers <* sym '\n'

possible :: Int -> Int -> Int -> Int
possible a b i | ((i * a) - (i ^ 2)) > b = 1
            | otherwise = 0

numWays :: Race -> Int
numWays (a, b) = sum . map (possible a b) $ [0..a]

part1 :: Inp -> Int
part1 = product . map numWays

kerned :: [Race] -> Race
kerned records = (kern time, kern dist)
  where time = map getTime records
        getTime (a, _) = a
        dist = map getDist records
        getDist (_, b) = b
        kern = read . (>>= show)

part2 :: [Race] -> Int
part2 = numWays . kerned

solve :: String -> IO ()
solve input = putStrLn "--- Day 06 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = fromMaybe (error "No Parse") . (=~ parse1)
