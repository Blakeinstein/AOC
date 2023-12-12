module Day01 (solve) where

import Control.Applicative (asum)

import Data.Char (isDigit, digitToInt, intToDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)

extractDigits :: String -> Int
extractDigits str
  | not (null digits) = makeTwoDigitNumber (head digits) (last digits)
  | otherwise = 0
  where
    digits = filter isDigit str
    makeTwoDigitNumber first last = digitToInt first * 10 + digitToInt last

part1 :: [String] -> Int
part1 = sum . map extractDigits

digits = [ ("one", 1), ("two", 2), ("three", 3), ("four", 4)
          , ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)
          ]
          <>
          [(pure (intToDigit x), x) | x <- [1..9]]

decodeDigit :: String -> Maybe Int
decodeDigit s = asum . map find $ digits
  where find (k, v) | k `isPrefixOf` s = Just v
                    | otherwise = Nothing

part2 :: [String] -> Int
part2 = sum . map (calibrationValue . mapMaybe decodeDigit . tails)
  where calibrationValue = (+) <$> ((* 10) . head) <*> last

solve :: String -> IO ()
solve input = putStrLn "--- Day 01 ---" >> print (part1 $ lines input) >> print (part2 $ lines input)